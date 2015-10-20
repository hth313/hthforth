{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternGuards, ScopedTypeVariables,  TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes, NoMonomorphismRestriction #-}
{- |

   The Forth interpreter.

-}

module Language.Forth.Interpreter (initialState, initialVarStorage, quit) where

import Numeric
import Control.Exception (try)
import Control.Applicative
import Control.Lens hiding (over)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Bits as Bits
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Word
import qualified Data.Map as Map
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V
import System.Console.Haskeline
import System.Exit
import System.IO
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as L
import Language.Forth.Interpreter.Address
import Language.Forth.Interpreter.CellMemory
import Language.Forth.Interpreter.DataField
import Language.Forth.CellVal
import Language.Forth.CrossCompiler
import Language.Forth.Dictionary
import Language.Forth.Machine
import Language.Forth.Primitive
import Language.Forth.StreamFile
import Language.Forth.Target
import Language.Forth.Target.CortexM (codeGenerateCortexM)
import Language.Forth.Target.MSP430 (codeGenerateMSP430)
import Language.Forth.Word
import Translator.Symbol
import Translator.Assembler.InstructionSet
import Translator.Assembler.Generate (IM)
import Util.Memory
import Prelude hiding (drop, until, repeat)
import qualified Prelude as Prelude


initialState target =
  FState [] [] [] target dict IntMap.empty wids [] Map.empty icompiler targetDictionary
    where (dict, wids) = interpreterDictionary

icompiler = Compiler defining icompile ilitComma xcompileBranch xcompileBranch irecurse istartDefining
                     icloseDefining abortDefining imm ireserveSpace
    where defining = isJust._idefining._dict
          abortDefining = dict.idefining.~Nothing
          imm = dict.idict.latest._Just.immediateFlag.~True

initialVarStorage = gets _target >>=
  \t -> let f (wid, val) =
              let field@(DataField cm) = newDataField t wid (bytesPerCell t)
              in  putField wid (DataField $ writeCell val (Addr wid 0) cm)
            g (wid, sz) = putField wid (newBuffer wid sz)
        in do
          mapM_ f [(stateWId, 0), (toInWId, 0),
                   (inputLineWId, 0), (inputLineLengthWId, 0),
                   (sourceIDWid, 0)]
          mapM_ g [(tregWid, 100)]

interpreterDictionary :: (IDict (FM a ()), [WordId])
interpreterDictionary = (IDict dict Nothing, wids)
  where (dict, wids) = newDictionary extras
        extras = do
          addWord "ROT" rot
          addWord "EVALUATE" evaluate
          addWord "FALSE" false
          addWord "TRUE" true
          addWord "STATE" state
          addWord ">IN" toIn
          addWord "#INBUF" inputBuffer
          addWord "INPUT-LINE" inputLine
          addWord "#INPUT-LINE" inputLineLength
          addWord "SOURCE-ID" sourceID
          addWord "\\"   backslash >> makeImmediate
          addWord "QUIT" quit
          addWord "ABORT" abort
          addWord "INTERPRET" interpret
          addWord ":" colon
          addWord ";" semicolon >> makeImmediate
          addWord "CREATE" create
          addWord "DOES>" does
          addWord "COMPILE," compileComma
          addWord "IMMEDIATE" immediate
          addWord "HERE" here
          addWord "BYE" (liftIO exitSuccess)
          addWord "LOAD-SOURCE" loadSource
          addWord "+!"   plusStore
          addWord "IF" xif >> makeImmediate
          addWord "ELSE" xelse >> makeImmediate
          addWord "THEN" xthen >> makeImmediate
          addWord "DO" xdo >> makeImmediate
          addWord pdoName pdo
          addWord "LOOP" loop >> makeImmediate
          addWord ploopName ploop
          addWord "+LOOP" plusLoop >> makeImmediate
          addWord pploopName pplusLoop
          addWord "LEAVE" leave
          addWord "I" rfetch
          addWord "BEGIN" begin >> makeImmediate
          addWord "UNTIL" until >> makeImmediate
          addWord "AGAIN" again >> makeImmediate
          addWord "WHILE" while >> makeImmediate
          addWord "REPEAT" repeat >> makeImmediate
          addWord "EMIT" emit
          addWord "MOVE" move
          addWord "FIND" find
          addWord "TREG" treg
          addWord "PAD" pad
          addWord "STRING," compileString
          addWord "LIT," (dpop >>= \x -> cprim1 litComma x)
          addWord "ALLOT" allot
          addWord ">BODY" toBody
          addWord "ACCEPT" accept
          addWord "ALIGN" align
          addWord "ALIGNED" aligned
          addWord "DEPTH" depth
          addWord "KEY" key
          addWord "RECURSE" (cprim recurse)
          addWord "CROSS-COMPILER" crossCompileSetup
          addWord "INTERPRETER" interpreterCompileSetup
          addWord "DUMP-CORTEXM" (targetCodegen codeGenerateCortexM)
          addWord "DUMP-MSP430" (targetCodegen codeGenerateMSP430)


-- | Foundation of the Forth interpreter
instance Primitive (FM a ()) where
  exit = rpop >>= \case
           IP ip' -> do
             modify $ \s -> s { _ip = ip' }
             next
           otherwise -> abortMessage "IP not on rstack"
  execute = call =<< dpop
  swap = updateState f  where
     f s | s0 : s1 : ss <- _stack s = newState s { _stack = s1 : s0 : ss }
         | otherwise = emptyStack s
  drop = updateState f  where
    f s | _ : ss <- _stack s = newState s { _stack = ss }
        | otherwise = emptyStack s
  dup = updateState f  where
    f s | ss@(s0 : _) <- _stack s = newState s { _stack = s0 : ss }
        | otherwise = emptyStack s
  over = updateState f  where
    f s | ss@(_ : s1 : _) <- _stack s = newState s { _stack = s1 : ss }
        | otherwise = emptyStack s
  tor = updateState f  where
     f s | s0 : ss <- _stack s = newState s { _stack = ss, _rstack = s0 : _rstack s }
         | otherwise = emptyStack s
  rto = updateState f  where
    f s | r0 : rs <- _rstack s = newState s { _rstack = rs, _stack = r0 : _stack s }
        | otherwise = emptyStack s
  rfetch = updateState f  where
    f s | r0 : _ <- _rstack s = newState s { _stack = r0 : _stack s }
        | otherwise = emptyStack s
  cfetch = cfetch'
  fetch = fetch'
  cstore = cstore'
  store = store'
  plus  = binary (+)
  minus = binary (-)
  and   = binary (Bits..&.)
  or    = binary (Bits..|.)
  xor   = binary Bits.xor

  twoStar = docol [dup, plus, exit]
  twoSlash = updateState f  where
    f s | Val x : ss <- _stack s = newState s { _stack = Val (x `Bits.shiftR` 1) : ss }
        | otherwise = abortWith "bad input to 2/" s
  lshift = updateState f  where
    f s | Val n : Val x : ss <- _stack s =
            newState s { _stack = Val (x `Bits.shiftL` (fromIntegral n)) : ss }
        | otherwise = abortWith "bad input to LSHIFT" s
  rshift = updateState f  where
    f s | Val n : x@Val{} : ss <- _stack s =
            let x' = unsigned x `Bits.shiftR` (fromIntegral n)
            in newState s { _stack = Val (fromIntegral x') : ss }
        | otherwise = abortWith "bad input to RSHIFT" s

  zerop = updateState $ \s -> case _stack s of
                                (Val 0) : ss         -> newState s { _stack = trueVal  : ss }
                                Val{} : ss           -> newState s { _stack = falseVal : ss }
                                Address Nothing : ss -> newState s { _stack = trueVal : ss }
                                Address{} : ss       -> newState s { _stack = falseVal : ss }
                                otherwise            -> emptyStack s
  lt0 = updateState f  where
    f s | (Val n) : ss <- _stack s =
            let flag | n < 0 = trueVal
                     | otherwise = falseVal
            in newState s { _stack = flag : ss }
        | null (_stack s) = emptyStack s
        | otherwise = abortWith "bad input to 0<" s
  constant = dpop >>= \x -> docol [xword, create' (const $ lit x) (DOCONST $ cellToExpr x), exit]

  umstar = umstar'
  ummod = ummod'

lit (Text text) = modify (\s ->
                            let u = fromIntegral $ C.length text
                                (caddr, s') = addrString text s
                            in  s' { _stack = Val u : caddr : _stack s' }) >> next
lit val = dpush val >> next

-- | Invoke compiler primitive, intercepting the error condition that
--   we are currently not in compilation mode.
cprim cf = updateState f  where
  f s | isdefining s = newState $ s^.compilerFuns.cf $ s
      | otherwise = notDefining s

cprim1 cf arg = updateState f  where
  f s | isdefining s = newState $ (s^.compilerFuns.cf) arg s
      | otherwise = notDefining s

-- variables
state           = litAdr stateWId
toIn            = litAdr toInWId
inputBuffer     = litAdr inputBufferWId
inputLine       = litAdr inputLineWId
inputLineLength = litAdr inputLineLengthWId
sourceID        = litAdr sourceIDWid

false = lit falseVal
true = lit trueVal

rot = updateState f  where
  f s | s0 : s1 : s2 : ss <- _stack s = newState s { _stack = s2 : s0 : s1 : ss }
      | otherwise = emptyStack s

treg = litAdr tregWid
pad = docol [treg, lit (Val 64), plus, exit]

-- Control structures
xif   = docol [here, icompileBranch branch0, exit]
xelse = docol [here, icompileBranch branch, here, rot, backpatch, exit]
xthen = docol [here, swap, backpatch, exit]

toSymbol = nameMangle . C.unpack

xdo = docol [cprim1 compile (XT Nothing (Just pdo) (Just $ toSymbol pdoName)), here, exit]
loop = xloop (XT Nothing (Just ploop) (Just $ toSymbol ploopName))
plusLoop = xloop (XT Nothing (Just pplusLoop) (Just $ toSymbol pploopName))
leave = updateState f  where
  f s | _ : rs@(limit : _) <- _rstack s = newState s { _rstack = limit : rs }
      | otherwise = emptyStack s
begin = here
until = docol [here, icompileBranch branch0, backpatch, exit]
again = docol [here, icompileBranch branch, backpatch, exit]
while = docol [here, icompileBranch branch0, exit]
repeat = docol [swap, here, icompileBranch branch, backpatch, here, swap, backpatch, exit]

quit = ipdo [ (modify (\s -> s { _rstack = [], _stack = Val 0 : _stack s }) >> next),
              sourceID, store, mainLoop ]

plusStore = docol [dup, fetch, rot, plus, swap, store, exit]

create = docol [xword, create' docol CREATE, exit]
colon = docol [lit (Val (-1)), state, store, xword, create' docol DOCOL, exit]
semicolon = docol [cprim1 compile (XT Nothing (Just exit) (Just $ toSymbol exitName)), lit (Val 0), state, store, reveal, exit]
compileComma = dpop >>= \x -> cprim1 compile x
immediate = updateState $ \s -> newState $ s^.compilerFuns.setImmediate $ s

does = updateState f  where
  f s | IP ip' : rs <- _rstack s, Just wid <- s^?dict.idict.latest._Just.wordId =
          let dobody = docol (litAdr wid : _ip s)
          in newState $ s & dict.idict.latest._Just.doer.~dobody & ip.~ip' & rstack.~rs
      | null (_rstack s) = emptyStack s
      | otherwise = abortWith "IP not on rstack" s

-- Helper function that compile the ending loop word
xloop xt = docol [cprim1 compile xt, here, icompileBranch branch0, backpatch, exit]

-- | Runtime words for DO-LOOPs
pdo = updateState f  where
  f s | s0 : s1 : ss <- _stack s = newState s { _stack = ss,
                                               _rstack = s0 : s1 : _rstack s }
      | otherwise = emptyStack s
ploop = updateState $ rloopHelper (Val 1 +)
pplusLoop = updateState $ \s -> case _stack s of
                                  n : ss -> rloopHelper (n+) (s { _stack = ss })
rloopHelper f s
  | i : r2@(limit : rs) <- _rstack s =
    let i' = f i
    in newState $ if i' < limit
                  then s { _rstack = i' : r2,
                           _stack = falseVal : _stack s }
                  else s { _rstack = rs,
                           _stack = trueVal : _stack s }
  | otherwise = emptyStack s

-- | Helper for arithmetics
binary :: (CV a -> CV a -> CV a) -> FM a ()
binary op = updateState f  where
  f s | op1 : op2 : ss <- _stack s = newState s { _stack = op2 `op` op1 : ss }
      | otherwise = emptyStack s

-- | Convert a cell value to a large unsigned number
unsigned :: CV a -> Word64
unsigned c@(Val x) =
  let (ux :: Word64) = fromIntegral x
      Just bitsize = bitSizeMaybe c
      bitmask = (1 `Bits.shiftL` bitsize) - 1
  in ux Bits..&. bitmask

-- | Call given colon definition body.
docol xs = modify (\s -> s { _rstack = IP (_ip s) : _rstack s, _ip = xs }) >> next

branch = ipdo
branch0 loc = dpop >>= \n -> if | isZero n -> ipdo loc
                                | otherwise  -> next

-- | Replace what we are interpreting with given slice of code.
--   Typically used for implementing branches and setting the
--   main loop.
ipdo ip' = modify (\s -> s { _ip = ip' }) >> next

-- | Search dictionary for given named word.
searchDict :: ByteString -> FM a (Maybe (ForthWord (FM a())), Maybe Symbol)
searchDict n = gets (\s -> (f (s^.dict.idict.latest),
                               nameSymbol <$> f ((arbitraryTargetDict s)^.tdict.latest)))
  where f jw@(Just word) | n == word^.name = jw
                         | otherwise = f (word^.link)
        f Nothing = Nothing

-- | Main loop for the interpreter
mainLoop = do
  mline <- lift $ getInputLine ""
  case mline of
    Nothing -> return ()
    Just input ->
        let line = C.pack input
        in ipdo [ putField inputBufferWId (textBuffer inputBufferWId line) >> next,
                  lit (Val 0), toIn, store,
                  litAdr inputBufferWId, inputLine, store,
                  lit (Val $ fromIntegral $ C.length line), inputLineLength, store,
                  interpret, liftIO (putStrLn "ok") >> next, mainLoop]

interpret = docol begin
  where begin = xword : dup : cfetch : zerop : branch0 lab1 : drop : exit : lab1
        lab1 = find : dup : zerop : branch0 lab2 : drop : parseNumber : state : fetch : branch0 begin : compileComma : branch begin : lab2
        lab2 = lit (Val 1) : minus : zerop : branch0 lab3 : execute : branch begin : lab3
        lab3 = state : fetch : zerop : branch0 skip1 : execute : branch begin : skip1
        skip1 = [compileComma, branch begin]
        parseNumber = dpop >>= countedText >>= parse where
          parse bs = case readSigned readDec text of
                       [(x,"")] -> lit $ Val x
                       otherwise -> abortMessage $ text ++ " ?"
                       where text = C.unpack bs

evaluate = docol [inputLine, fetch, tor,              -- save input specification
                  inputLineLength, fetch, tor,
                  sourceID, fetch, tor,
                  toIn, fetch, tor,
                  lit (Val (-1)), sourceID, store,    -- set SOURCE-ID to -1
                  inputLineLength, store,             -- set new SOURCE specification
                  inputLine, store,
                  lit (Val 0), toIn, store,           -- clear >IN
                  interpret,
                  rto, toIn, store,                   -- restore input specification
                  rto, sourceID, store,
                  rto, inputLineLength, store,
                  rto, inputLine, store, exit]

-- | Insert the field contents of given word
putField :: WordId -> DataField (FM a ()) -> FM a ()
putField wid field = modify $ \s -> s { _variables = IntMap.insert (unWordId wid) field  (_variables s) }

-- | Push the field address of a word on stack
litAdr :: WordId -> FM a ()
litAdr = lit . adrcv

-- | Addressable value, pointing to the first address of the datafield of
--   given word.
adrcv wid = Address (Just $ Addr wid 0)

-- | Forth level error handling.
abort = docol [modify (\s -> (s^.compilerFuns.abortDefining $ s) & stack.~[]) >> next,
               lit (Val 0), state, store, quit]

emptyStack = abortWith "empty stack"
notDefining = abortWith "not defining"
abortWith msg s = return (Left msg, s)
abortMessage msg = liftIO (putStrLn msg) >> abort

-- | Step the colon body and execute next word in it.
next = do x <- StateT $ \s -> let (x:xs) = _ip s
                              in return (x, s { _ip = xs } )
          x

-- | Invoke an execution token.
call :: CV a -> FM a ()
call (XT _ (Just a) _) = a
call (XT _ _ Just{}) = abortMessage "xt only known to a target"
call _ = abortMessage "not an execution token"

-- | Data stack primitives
dpush :: CV a -> FM a ()
dpush val = modify $ \s -> s { _stack = val : _stack s }

dpop :: FM a (CV a)
dpop = updateStateVal (Val 0) f  where
  f s | t:ts <- _stack s = return (Right t, s { _stack = ts })
      | otherwise = emptyStack s

-- | Return stack primitives
rpop :: FM a (CV a)
rpop = updateStateVal (Val 0) f  where
  f s | t:ts <- _rstack s = return (Right t, s { _rstack = ts })
      | otherwise = emptyStack s

-- | State updater that can handle aborts and that automatically do 'next'
updateState f = StateT f >>= \case
                  Left msg -> abortMessage msg
                  Right () -> next

-- | State updater building block that can handle abort and that can
--   be used together with other actions. The last action need to
--   be 'next'.
updateStateVal x f = StateT f >>= \case
                        Left msg -> abortMessage msg >> return x
                        Right y -> return y

newState s = return (Right (), s)

cfetch' = updateState f  where
  f s | Address (Just adr@(Addr wid _)) : rest <- _stack s =
          case IntMap.lookup (unWordId wid) (_variables s) of
            Just (BufferField buf) | Just val <- read8 adr buf ->
              let c = Val (fromIntegral val)
              in  newState s { _stack = c : rest }
            Nothing -> abortWith "C@ - no valid address" s
            Just (DataField cm) | Just (Byte x) <- read8CM adr cm ->
              newState s { _stack = Val (fromIntegral x) : rest }
            otherwise -> abortWith "C@ - no defined value found in cell memory" s
      | null (_stack s) = emptyStack s
      | otherwise = abortWith "bad C@ address" s

cstore' = do
  action <- updateStateVal (return ()) $ \s ->
    case _stack s of
      Address (Just adr@(Addr wid i)) : Val val : rest ->
        case IntMap.lookup (unWordId wid) (_variables s) of
          Just (BufferField bm) ->
            return (Right (write8 (fromIntegral val) adr bm), s { _stack = rest })
          Just (DataField df) ->
            return (Right (return ()),
                    s { _stack = rest,
                        _variables = IntMap.insert (unWordId wid)
                                      (DataField $ write8CM (fromIntegral val) adr df)
                                      (_variables s) })
          otherwise -> abortWith "missing data field" s
      [] -> emptyStack s
      [x] -> abortWith "no value to C! to" s
      x:_ -> abortWith "cannot C! to non-address" s
  liftIO action
  next

fetch' = updateState f  where
  f s | Address (Just adr@(Addr wid off)) : rest <- _stack s =
          case IntMap.lookup (unWordId wid) (_variables s) of
            Just (DataField cm) ->
                case readCell adr cm of
                  Just x -> newState s { _stack = x : rest }
                  _ | validAddressCM adr cm -> abortWith "uninitialized access in data field" s
                    | otherwise -> abortWith "@ outside data field" s
            Just (BufferField mem) -> abortWith "@ in buffer field" s
            Nothing -> abortWith "no data field" s
      | null (_stack s) = emptyStack s
      | otherwise = abortWith "bad address given to @" s

store' = updateState f  where
  f s | Address (Just adr@(Addr wid i)) : val : rest <- _stack s,
        Just (DataField cm) <- IntMap.lookup (unWordId wid) (_variables s) =
          newState s { _variables = IntMap.insert (unWordId wid) (DataField $ writeCell val adr cm)
                       (_variables s),
                       _stack = rest }
      | null (_stack s) = emptyStack s
      | otherwise = abortWith "Bad arguments to !" s

-- | Given a counted string, extract the actual text as an individual ByteString.
countedText :: CV a -> FM a ByteString
countedText (Address (Just (Addr wid off))) = updateStateVal "" $ \s ->
    case IntMap.lookup (unWordId wid) (_variables s) of
      Just (BufferField cmem) ->
          let count = fromIntegral $ B.index (chunk cmem) off
          in return (Right $ B.take count $ B.drop (off + 1) (chunk cmem), s)
      otherwise -> abortWith "expected address pointing to char buffer" s
countedText _ = abortMessage "expected address" >> return B.empty

xt word = XT (_wordId <$> word) (_doer <$> word)

-- | Find the name (counted string) in the dictionary
--   ( c-addr -- c-addr 0 | xt 1 | xt -1 )
find = do
  caddr <- dpop
  (word, sym) <- searchDict =<< countedText caddr
  modify $ \s ->
    let imm = case _immediateFlag <$> word of
                Just True ->  1
                otherwise -> -1
    in if (isJust word || isJust sym) then s { _stack = Val imm : xt word sym : _stack s }
       else s { _stack = Val 0 : caddr : _stack s }
  next

-- | Copy word from given address with delimiter to a special transient area.
--   ( "<chars>ccc<char>" -- c-addr )
xword = docol [inputLine, fetch, toIn, fetch, plus, parseName, toIn, plusStore, exit]
  where
    parseName =   -- ( "<spaces>ccc<space>" -- ctransbuf n )
      updateState  $ \s ->
         case _stack s of
           Address (Just (Addr wid off)) : ss
             | Just (BufferField cmem) <- IntMap.lookup (unWordId wid) (_variables s) ->
                 let start = B.drop off (chunk cmem)
                     (skipCount, nameStart) = skipSpaces 0 start
                     skipSpaces n bs
                       | not (C.null bs), C.head bs <= ' ' = skipSpaces (n + 1) (B.tail bs)
                       | otherwise = (n, bs)
                     name = C.takeWhile (> ' ') nameStart
                     nameLength = C.length name
                     pastdelim | name /= nameStart = 1  -- trailing delimiter present
                               | otherwise = 0
                     inAdjust = skipCount + nameLength + pastdelim
                     countedField = textBuffer wordBufferWId (B.cons (fromIntegral nameLength) name)
                 in newState s { _stack = Val (fromIntegral inAdjust) : Address (Just $ Addr wordBufferWId 0) : ss,
                                 _variables = IntMap.insert (unWordId wordBufferWId) countedField (_variables s) }
           otherwise -> abortWith "parseName failed" s

-- | Interpreter - Compile given cell value
icompile adr@Address{}      = tackOn $ WrapA $ lit adr
icompile val@Val{}          = tackOn $ WrapA $ lit val
icompile val@Text{}         = tackOn $ WrapA $ lit val
icompile (XT _ (Just a) _ ) = tackOn $ WrapA $ a

-- | Interpreter - Compile a cell value from the stack.
ilitComma x = tackOn $ WrapA $ lit x

-- | Compile a branch instruction. Branches need special handling when
--   the colon definition is finailized.
icompileBranch :: ([FM a ()] -> FM a ()) -> FM a ()
icompileBranch dest = updateState f  where
  f s | isdefining s = newState $ tackOn (WrapB dest) s
      | otherwise = notDefining s

xcompileBranch _ = error "do not call xcompileBranch"

irecurse = tackOn WrapRecurse

tackOn x = dict.idefining._Just.compileList%~(<>V.singleton x)

isdefining s = (s^.compilerFuns.defining) s

-- | Helper for create. Open up for defining a word assuming that the name of the
--   word can be found on top of stack.
--   ( caddr -- )  of word name to be created
create' finalizer usingCreate = updateState f  where
  f s | isdefining s = abortWith "already compiling" s
      | Address (Just (Addr awid off)) : ss <- _stack s,
        Just (BufferField cmem) <- IntMap.lookup (unWordId awid) (_variables s) =
        let name = B.drop (1 + off) $ chunk cmem
        in newState $ (s^.compilerFuns.startDefining) (Create name finalizer usingCreate) (s & stack.~ss)
      | otherwise = abortWith "missing word name" s

istartDefining Create{..} s =
  let wid : wids' = s^.wids
      linkhead = s^.dict.idict.latest
      (variables', code, cl)
        | createStyle == CREATE = (IntMap.insert (unWordId wid) (newDataField (_target s) (unWordId wid) 0) (_variables s), V.fromList (map WrapA [litAdr wid, exit]), s^.compilerFuns.closeDefining)
        | otherwise = (_variables s, V.empty, reveal)
        where reveal = case createStyle of
                         DOCOL -> id  -- do not reveal immediately
                         otherwise -> s^.compilerFuns.closeDefining
      defining = IDefining code [] finalizer (ForthWord createName False linkhead wid abort)
  in cl $ s & variables.~variables' & wids.~wids' & dict.idefining.~(Just defining)

reveal = updateState f  where
  f s | isdefining s = newState $ s^.compilerFuns.closeDefining $ s
      | otherwise = notDefining s

-- | Close the word being defined.
icloseDefining :: FState t -> FState t
icloseDefining s =
  let word = (_definingWord defining) { _doer = (_defineFinalizer defining) cs }
      Just defining = s^.dict.idefining
      vs = _compileList defining
      -- Compile the branch instructions using the patch list provided by
      -- backpatch function. We rely on lazy evaluation here and insert
      -- branch destinations where lazy functions that will end up dropping
      -- 'dest' elements from final colon list.
      cs = map unWrap $ V.toList $ (V.//) vs (map f $ _patchList defining)
      f (loc, dest) =
        let branchInstr | WrapB b <- (V.!) vs loc = WrapA $ b (Prelude.drop dest cs)
        in  (loc, branchInstr)
      unWrap WrapRecurse = branch cs
      unWrap (WrapA a) = a
  in s & dict.idefining.~Nothing & dict.idict.latest.~(Just word)

here = updateState f  where
  f s | Just def <- s^.dict.idefining =
          let wid = def^.definingWord.wordId
          in newState s { _stack = HereColon wid (V.length (_compileList def)) : _stack s }
      | Just word <- s^.dict.idict.latest, wid <- _wordId word,
        Just (DataField mem) <- IntMap.lookup (unWordId wid) (_variables s) =
          newState s { _stack = Address (Just (Addr wid (dpOffset mem))) : _stack s }
      | otherwise = abortWith "HERE only partially implemented" s

backpatch = updateState f  where
  f s | Just def <- s^.dict.idefining,
        HereColon _ loc : HereColon _ dest : ss <- s^.stack =
          newState $ s & stack.~ss & dict.idefining._Just.patchList%~((:) (loc, dest))
      | otherwise = notDefining s

backslash = docol body
  where body = toIn : fetch : inputLine : fetch : over : plus : inputLineLength : fetch : rot : minus : dup : branch0 found : loop
        loop = lit (Val 1) : minus : dup : branch0 eol : over : cfetch : lit (Val 10) : minus : branch0 found : swap : lit (Val 1) : plus : swap : branch loop : eol
        eol = drop : drop : inputLineLength : fetch : toIn : store : exit : found
        found = [inputLineLength, fetch, swap, minus, toIn, store, drop, exit]

popFilename :: FM a String
popFilename =
  updateStateVal "" $ \s ->
    case _stack s of
      Address (Just (Addr wid off)) : ss
        | Just (BufferField cmem) <- IntMap.lookup (unWordId wid) (_variables s),
          not (B.null $ chunk cmem) ->
            let len = fromIntegral $ B.head $ chunk cmem
                name = C.take len $ C.drop (1 + off) $ chunk cmem
            in return (Right (C.unpack name), s { _stack = ss })
        | otherwise -> abortWith "missing filename" s

loadSource = docol [xword, makeTempBuffer, evaluate, releaseTempBuffer, exit] where
  makeTempBuffer = do
    filename <- popFilename
    mc <- liftIO $ try $ readSourceFile filename
    case mc of
      Left (e :: IOException) -> abortMessage (show e)
      Right contents -> updateState $ \s ->
               let (handle, oldHandles', dict', s')
                     | null (_oldHandles s) = let w:ws = s^.wids
                                                  d = s^.dict.idict
                                              in (w, [], d, s & wids.~ws)
                     | otherwise = (head $ _oldHandles s, tail $ _oldHandles s, s^.dict.idict, s)
                   adr = Address (Just (Addr handle 0))
               in newState $ (s' & dict.idict.~dict')
                    { _oldHandles = oldHandles',
                      _rstack = adr : _rstack s,
                      _stack = Val (fromIntegral $ C.length contents) : adr : _stack s,
                      _variables = IntMap.insert (unWordId handle) (textBuffer handle contents) (_variables s) }

  releaseTempBuffer = updateState $ \s -> case _rstack s of
                                            Address (Just (Addr handle 0)) : rs ->
                                              newState s { _variables = IntMap.delete (unWordId handle) (_variables s),
                                                           _rstack = rs,
                                                           _oldHandles = handle : _oldHandles s }

-- | Generate code for a target
targetCodegen codeGenerate = docol [xword, dump, exit]
  where dump = do
          outputfile <- popFilename
          dict <- gets (\s -> s^.targetDict.tdict)
          let text = codeGenerate dict
          mres <- liftIO $ try $ withFile outputfile WriteMode (flip L.hPut text)
          case mres of
            Left (e :: IOException) -> abortMessage $ show e
            Right () -> next

crossCompileSetup = updateState $ \s -> newState $ s & compilerFuns.~crossCompiler
interpreterCompileSetup = updateState $ \s -> newState $ s & compilerFuns.~icompiler

-- | Compile a string literal. We expect to get a string pointer (caddr u) on
--   the stack pointing to some character buffer. Compile a string literal
--   that has the execution semantics to push the string back on stack.
--   For the interpreter we simply wrap it in a literal.
compileString = cprim1 compile =<< liftM Text stringlit
  where stringlit = updateStateVal "" $ \s ->
                      case _stack s of
                        Val n : Address (Just (Addr wid i)) : rest
                          | Just (BufferField bm) <- IntMap.lookup (unWordId wid) (_variables s) ->
                              let text = B.take (fromIntegral n) (B.drop i (chunk bm))
                              in return (Right text, s { _stack = rest } )
                          | otherwise -> abortWith "no text to compile" s
                        otherwise -> emptyStack s

-- | Make a string literal addressable on the fly.
addrString text s =
    case Map.lookup text (_stringLiterals s) of
      Just addr -> (Address (Just addr), s)
      Nothing ->
          let (k:ks) = s^.wids
              addr = Addr k 0
              in (Address (Just addr), (s & wids.~ks)
                                         { _stringLiterals = Map.insert text addr
                                                            (_stringLiterals s),
                                           _variables = IntMap.insert (unWordId k) (textBuffer k text) (_variables s) })

emit = dpop >>= emit1 >> next where
    emit1 (Val n) | n >= 0 = liftIO $ putStr [chr $ fromIntegral n]
    emit1 _ = liftIO $ putStr "?"

move = do
  mtuple <- updateStateVal Nothing $ \s ->
    case _stack s of
      Val count : Address (Just adrTo@(Addr widTo _iTo)) : Address (Just adrFrom@(Addr widFrom _iFrom)) : rest
          | Just (BufferField memTo) <- IntMap.lookup (unWordId widTo) (_variables s),
            Just (BufferField memFrom) <- IntMap.lookup (unWordId widFrom) (_variables s) ->
                return (Right (Just (count, adrFrom, memFrom, adrTo, memTo)), s { _stack = rest })
      xs | length xs < 3 -> emptyStack s
         | otherwise -> abortWith "illegal arguments to MOVE" s
  case mtuple of
    Nothing -> return ()
    Just (count, adrFrom, memFrom, adrTo, memTo) ->
        liftIO $ blockMove (fromIntegral count) adrFrom memFrom adrTo memTo
  next

allot = updateState f  where
  f s | Val n:ss <- _stack s = newState $ (s^.compilerFuns.reserveSpace) n (s & stack.~ss)
      | null (_stack s) = emptyStack s
      | otherwise = abortWith "ALLOT requires integer value" s

ireserveSpace :: Cell -> FState a -> FState a
ireserveSpace n s = s & variables%~(IntMap.insert (unWordId wid) (DataField mem1))
  where Just word = s^.dict.idict.latest
        wid = word^.wordId
        Just (DataField mem) = IntMap.lookup (unWordId wid) (s^.variables)
        (offset, mem1) = updateDataPointer (fromIntegral n +) mem

umstar' = updateState f  where
  f s | n1@Val{} : n2@Val{} : ss <- _stack s =
        let prod = unsigned n1 * unsigned n2
            Just bitsize = bitSizeMaybe n1
            low = mask prod
            high = mask $ prod `Bits.shiftR` bitsize
            mask x =  fromIntegral $ x Bits..&. ((1 `Bits.shiftL` bitsize) - 1)
        in newState s { _stack = Val high : Val low : ss }
      | otherwise = abortWith "bad input to UM*" s

ummod' = updateState f  where
  f s | divisor@Val{} : hi@Val{} : lo@Val{} : ss <- _stack s =
      let dividend = unsigned lo Bits..|. (unsigned hi `Bits.shiftL` bitsize)
          Just bitsize = bitSizeMaybe divisor
          (quot, rem) = dividend `quotRem` unsigned divisor
      in newState s { _stack = Val (fromIntegral quot) : Val (fromIntegral rem) : ss }
    | otherwise = abortWith "bad input to UM/MOD" s

toBody = updateState f  where
  f s | XT (Just wid) a _ : ss <- _stack s = newState s { _stack = adrcv wid : ss }
      | otherwise = abortWith "bad input to >BODY" s

accept =
  let f s | Val n : caddr@Address{} : ss <- _stack s =
              return (Right (fromIntegral n, caddr), s { _stack = ss })
          | otherwise = abortWith "bad arguments to ACCEPT, or nu buffer destination" s
      g len s = newState s { _stack = Val (fromIntegral len) : _stack s }
  in do
    (n, caddr) <- updateStateVal (0, Address Nothing) f
    text <- liftM (C.take n) (liftIO C.getLine)
    copyTextBlock caddr text
    updateState (g $ C.length text)

-- Copy given chunk of text to given address. This will handle writing to
-- either cell memory and buffer memory.
copyTextBlock (Address (Just adrTo@(Addr wid _))) text =
  let f s = case IntMap.lookup (unWordId wid) (_variables s) of
              Nothing -> abortWith "missing data field" s
              Just (BufferField memTo) | validAddress adrTo memTo,
                                         validAddress adrEnd memTo ->
                return (Right (liftIO (blockMoveText text adrTo memTo)), s)
              Just (DataField cm) | validAddressCM adrTo cm,
                                    validAddressCM adrEnd cm ->
                return (Right (putField wid (DataField $ blockMoveTextCM text adrTo cm)), s)
              otherwise -> abortWith "address outside allocated area" s
      adrEnd = addAddress adrTo (B.length text - 1)
  in do
    copyAction <- updateStateVal (return ()) f
    copyAction

align = updateState f  where
  f s | Just word <- s^.dict.idict.latest,
        Just (DataField mem) <- IntMap.lookup (unWordId (_wordId word)) (_variables s) =
          let mem' = DataField $ alignDP mem (_target s)
          in newState s { _variables = IntMap.insert (unWordId (_wordId word)) mem' (_variables s) }
      | otherwise = abortWith "cannot ALIGN, dp no valid" s

aligned = updateState f  where
  f s | Address (Just (Addr wid off)) : ss <- _stack s =
          newState s { _stack = Address (Just (Addr wid (alignOffset off (_target s)))) : ss }
      | otherwise = abortWith "ALIGNED only words on addresses" s

depth = updateState f  where
  f s = newState s { _stack = Val (fromIntegral $ length $ _stack s) : _stack s }

key =
  let withRawInput action = do
         buffering <- hGetBuffering stdin
         echo <- hGetEcho stdin
         hSetBuffering stdin NoBuffering
         hSetEcho stdin False
         result <- action
         hSetBuffering stdin buffering
         hSetEcho stdin echo
         return result
      readValidChar = do
        c <- getChar
        if valid c then return c else readValidChar
          where valid c = c >= ' ' && c <= '~'
  in do
    c <- liftIO $ withRawInput readValidChar
    updateState $ \s -> newState s { _stack = Val (fromIntegral $ ord c) : _stack s }
