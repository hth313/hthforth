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
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Bits as Bits
import Data.Char
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
import Translator.Assembler.InstructionSet
import Translator.Assembler.Generate (IM)
import Util.Memory
import Prelude hiding (drop, until, repeat)
import qualified Prelude as Prelude

initialState target =
  FState [] [] [] target interpreterDictionary IntMap.empty [] Map.empty icompiler Nothing Nothing

icompiler = Compiler icompile ilitComma xcompileBranch xcompileBranch irecurse closeDefining

initialVarStorage :: FM a ()
initialVarStorage = gets target >>=
  \t -> let f (wid, val) =
              let field@(DataField cm) = newDataField t wid (bytesPerCell t)
              in  putField wid (DataField $ writeCell val (Addr wid 0) cm)
            g (wid, sz) = putField wid (newBuffer wid sz)
        in do
          mapM_ f [(stateWId, 0), (toInWId, 0),
                   (inputLineWId, 0), (inputLineLengthWId, 0),
                   (sourceIDWid, 0)]
          mapM_ g [(tregWid, 100)]

interpreterDictionary :: Dictionary (FM a ())
interpreterDictionary = newDictionary extras
  where extras = do
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
          addWord "SMUDGE" smudge
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
          addWord "(DO)" pdo
          addWord "LOOP" loop >> makeImmediate
          addWord "(LOOP)" ploop
          addWord "+LOOP" plusLoop >> makeImmediate
          addWord "(+LOOP)" pplusLoop
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
          addWord "LIT," (dpop >>= \x -> cprim (flip litComma x))
          addWord "ALLOT" allot
          addWord ">BODY" toBody
          addWord "ACCEPT" accept
          addWord "ALIGN" align
          addWord "ALIGNED" aligned
          addWord "DEPTH" depth
          addWord "KEY" key
          addWord "RECURSE" (cprim recurse)
          addWord "CROSS-COMPILER" crossCompileSetup
          addWord "DUMP-CORTEXM" (targetCodegen codeGenerateCortexM)
          addWord "DUMP-MSP430" (targetCodegen codeGenerateMSP430)


-- | Foundation of the Forth interpreter
instance Primitive (FM a ()) where
  exit = rpop >>= \case
           IP ip' -> do
             modify $ \s -> s { ip = ip' }
             next
           otherwise -> abortMessage "IP not on rstack"
  execute = call =<< dpop
  lit (Text text) = modify (\s ->
                       let u = fromIntegral $ C.length text
                           (caddr, s') = addrString text s
                       in  s' { stack = Val u : caddr : stack s' }) >> next
  lit val = dpush val >> next
  swap = updateState f  where
     f s | s0 : s1 : ss <- stack s = newState s { stack = s1 : s0 : ss }
         | otherwise = emptyStack s
  drop = updateState f  where
    f s | _ : ss <- stack s = newState s { stack = ss }
        | otherwise = emptyStack s
  dup = updateState f  where
    f s | ss@(s0 : _) <- stack s = newState s { stack = s0 : ss }
        | otherwise = emptyStack s
  over = updateState f  where
    f s | ss@(_ : s1 : _) <- stack s = newState s { stack = s1 : ss }
        | otherwise = emptyStack s
  tor = updateState f  where
     f s | s0 : ss <- stack s = newState s { stack = ss, rstack = s0 : rstack s }
         | otherwise = emptyStack s
  rto = updateState f  where
    f s | r0 : rs <- rstack s = newState s { rstack = rs, stack = r0 : stack s }
        | otherwise = emptyStack s
  rfetch = updateState f  where
    f s | r0 : _ <- rstack s = newState s { stack = r0 : stack s }
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
    f s | Val x : ss <- stack s = newState s { stack = Val (x `Bits.shiftR` 1) : ss }
        | otherwise = abortWith "bad input to 2/" s
  lshift = updateState f  where
    f s | Val n : Val x : ss <- stack s =
            newState s { stack = Val (x `Bits.shiftL` (fromIntegral n)) : ss }
        | otherwise = abortWith "bad input to LSHIFT" s
  rshift = updateState f  where
    f s | Val n : x@Val{} : ss <- stack s =
            let x' = unsigned x `Bits.shiftR` (fromIntegral n)
            in newState s { stack = Val (fromIntegral x') : ss }
        | otherwise = abortWith "bad input to RSHIFT" s

  zerop = updateState $ \s -> case stack s of
                                (Val 0) : ss         -> newState s { stack = trueVal  : ss }
                                Val{} : ss           -> newState s { stack = falseVal : ss }
                                Address Nothing : ss -> newState s { stack = trueVal : ss }
                                Address{} : ss       -> newState s { stack = falseVal : ss }
                                otherwise            -> emptyStack s
  lt0 = updateState f  where
    f s | (Val n) : ss <- stack s =
            let flag | n < 0 = trueVal
                     | otherwise = falseVal
            in newState s { stack = flag : ss }
        | null (stack s) = emptyStack s
        | otherwise = abortWith "bad input to 0<" s
  constant = docol [xword, create' head False, compileComma, smudge, exit]

  umstar = umstar'
  ummod = ummod'

-- Forward declarations of Forth words implemented by the interpreter
xif, xelse, xthen, xdo, loop, plusLoop, leave, begin, until, again,
  repeat, while,
  interpret, plusStore, create, does, colon, semicolon, quit,
  compileComma, smudge, immediate, pdo, ploop, pplusLoop,
  here, backpatch, backslash, loadSource, emit, treg, pad,
  allot, umstar', ummod', rot, evaluate, false, true, key,
  state, sourceID, toIn, inputBuffer, inputLine, inputLineLength,
  toBody, accept, align, aligned, depth
  :: FM a ()


-- | Invoke a compilation primitve for the interpreter only
-- iprim :: (Cell cell, Compiler cell (FM cell t ()) (Defining cell (FM cell t ()))) =>
--         (Defining cell (FM cell t ()) -> Defining cell (FM cell t ())) -> FM cell t ()
-- cprim :: Cell cell => (forall d. (d -> d)) -> FM cell t ()
cprim cf = updateState f  where
  f s = case defining s of
          Just{} -> newState s { defining = cf (compilerFuns s) <$> defining s }
          Nothing -> notDefining s

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
  f s | s0 : s1 : s2 : ss <- stack s = newState s { stack = s2 : s0 : s1 : ss }
      | otherwise = emptyStack s

treg = litAdr tregWid
pad = docol [treg, lit (Val 64), plus, exit]

-- Control structures
xif   = docol [here, icompileBranch branch0, exit]
xelse = docol [here, icompileBranch branch, here, rot, backpatch, exit]
xthen = docol [here, swap, backpatch, exit]

xdo = docol [cprim (flip compile (XT Nothing pdo)), here, exit]
loop = xloop ploop
plusLoop = xloop pplusLoop
leave = updateState f  where
  f s | _ : rs@(limit : _) <- rstack s = newState s { rstack = limit : rs }
      | otherwise = emptyStack s
begin = here
until = docol [here, icompileBranch branch0, backpatch, exit]
again = docol [here, icompileBranch branch, backpatch, exit]
while = docol [here, icompileBranch branch0, exit]
repeat = docol [swap, here, icompileBranch branch, backpatch, here, swap, backpatch, exit]

quit = ipdo [ (modify (\s -> s { rstack = [], stack = Val 0 : stack s }) >> next),
              sourceID, store, mainLoop ]

plusStore = docol [dup, fetch, rot, plus, swap, store, exit]

create = docol [xword, create' docol True, exit]
colon = docol [lit (Val (-1)), state, store, xword, create' docol False, exit]
semicolon = docol [cprim (flip compile (XT Nothing exit)), lit (Val 0), state, store, smudge, exit]
compileComma = dpop >>= \x -> cprim (flip compile x)
immediate = updateState $ \s -> newState s { dict = setLatestImmediate (dict s) }

does = updateState f  where
  f s | IP ip' : rs <- rstack s =
      let Just word = latest (dict s)
          word' = word { doer = docol (litAdr (wordId word) : ip s) }
          dict' = (dict s) { latest = Just word' }
      in newState s { ip = ip',
                      dict = dict',
                      rstack = rs }
        | null (rstack s) = emptyStack s
        | otherwise = abortWith "IP not on rstack" s

-- Helper function that compile the ending loop word
xloop :: FM a () -> FM a ()
xloop a = docol [cprim (flip compile (XT Nothing a)), here, icompileBranch branch0, backpatch, exit]

-- | Runtime words for DO-LOOPs
pdo = updateState f  where
  f s | s0 : s1 : ss <- stack s = newState s { stack = ss,
                                               rstack = s0 : s1 : rstack s }
      | otherwise = emptyStack s
ploop = updateState $ rloopHelper (Val 1 +)
pplusLoop = updateState $ \s -> case stack s of
                                  n : ss -> rloopHelper (n+) (s { stack = ss })
rloopHelper f s
  | i : r2@(limit : rs) <- rstack s =
    let i' = f i
    in newState $ if i' < limit
                  then s { rstack = i' : r2,
                           stack = falseVal : stack s }
                  else s { rstack = rs,
                           stack = trueVal : stack s }
  | otherwise = emptyStack s

-- | Helper for arithmetics
binary :: (CV a -> CV a -> CV a) -> FM a ()
binary op = updateState f  where
  f s | op1 : op2 : ss <- stack s = newState s { stack = op2 `op` op1 : ss }
      | otherwise = emptyStack s

-- | Convert a cell value to a large unsigned number
unsigned :: CV a -> Word64
unsigned c@(Val x) =
  let (ux :: Word64) = fromIntegral x
      Just bitsize = bitSizeMaybe c
      bitmask = (1 `Bits.shiftL` bitsize) - 1
  in ux Bits..&. bitmask

-- | Call given colon definition body.
docol, branch, branch0 :: [FM a ()] -> FM a ()
docol xs = modify (\s -> s { rstack = IP (ip s) : rstack s, ip = xs }) >> next

branch = ipdo
branch0 loc = dpop >>= \n -> if | isZero n -> ipdo loc
                                | otherwise  -> next

-- | Replace what we are interpreting with given slice of code.
--   Typically used for implementing branches and setting the
--   main loop.
ipdo :: [FM a ()] -> FM a ()
ipdo ip' = modify (\s -> s { ip = ip' }) >> next

-- | Search dictionary for given named word.
searchDict :: ByteString -> FM a (Maybe (ForthWord (FM a ())))
searchDict n = gets (f . latest . dict)
  where f jw@(Just word) | n == name word = jw
                         | otherwise = f (link word)
        f Nothing = Nothing

-- | Main loop for the interpreter
mainLoop :: FM a ()
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
putField wid field = modify $ \s -> s { variables = IntMap.insert (unWordId wid) field  (variables s) }

-- | Push the field address of a word on stack
litAdr :: WordId -> FM a ()
litAdr = lit . adrcv

-- | Addressable value, pointing to the first address of the datafield of
--   given word.
adrcv wid = Address (Just $ Addr wid 0)

-- | Forth level error handling.
abort :: FM a ()
abort = docol [modify (\s -> s { stack = [], defining = Nothing }) >> next,
               lit (Val 0), state, store, quit]

emptyStack = abortWith "empty stack"
notDefining = abortWith "not defining"
abortWith msg s = return (Left msg, s)
abortMessage msg = liftIO (putStrLn msg) >> abort

-- | Step the colon body and execute next word in it.
next :: FM a ()
next = do x <- StateT $ \s -> let (x:xs) = ip s
                              in return (x, s { ip = xs } )
          x

-- | Invoke an execution token.
call :: CV a -> FM a ()
call (XT _ a) = a
call _ = abortMessage "not an execution token"

-- | Data stack primitives
dpush :: CV a -> FM a ()
dpush val = modify $ \s -> s { stack = val : stack s }

dpop :: FM a (CV a)
dpop = updateStateVal (Val 0) f  where
  f s | t:ts <- stack s = return (Right t, s { stack = ts })
      | otherwise = emptyStack s

-- | Return stack primitives
rpop :: FM a (CV a)
rpop = updateStateVal (Val 0) f  where
  f s | t:ts <- rstack s = return (Right t, s { rstack = ts })
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

cfetch' :: FM a ()
cfetch' = updateState f  where
  f s | Address (Just adr@(Addr wid _)) : rest <- stack s =
          case IntMap.lookup (unWordId wid) (variables s) of
            Just (BufferField buf) ->
              let c = Val $ fromIntegral $ read8 adr buf
              in  newState s { stack = c : rest }
            Nothing -> abortWith "C@ - no valid address" s
            Just (DataField cm) | Just (Byte x) <- read8CM adr cm ->
              newState s { stack = Val (fromIntegral x) : rest }
            otherwise -> abortWith "C@ - no byte found in cell memory" s
      | null (stack s) = emptyStack s
      | otherwise = abortWith "bad C@ address" s

cstore' :: FM a ()
cstore' = do
  action <- updateStateVal (return ()) $ \s ->
    case stack s of
      Address (Just adr@(Addr wid i)) : Val val : rest ->
        case IntMap.lookup (unWordId wid) (variables s) of
          Just (BufferField bm) ->
            return (Right (write8 (fromIntegral val) adr bm), s { stack = rest })
          Just (DataField df) ->
            return (Right (return ()),
                    s { stack = rest,
                        variables = IntMap.insert (unWordId wid)
                                      (DataField $ write8CM (fromIntegral val) adr df)
                                      (variables s) })
          otherwise -> abortWith "missing data field" s
      [] -> emptyStack s
      [x] -> abortWith "no value to C! to" s
      x:_ -> abortWith "cannot C! to non-address" s
  liftIO action
  next

fetch' :: FM a ()
fetch' = updateState f  where
  f s | Address (Just adr@(Addr wid off)) : rest <- stack s =
          case IntMap.lookup (unWordId wid) (variables s) of
            Just (DataField cm) ->
                case readCell adr cm of
                  Just x -> newState s { stack = x : rest }
                  _ | validAddressCM adr cm -> abortWith "uninitialized access in data field" s
                    | otherwise -> abortWith "@ outside data field" s
            Just (BufferField mem) -> abortWith "@ in buffer field" s
            Nothing -> abortWith "no data field" s
      | null (stack s) = emptyStack s
      | otherwise = abortWith "bad address given to @" s

store' :: FM a ()
store' = updateState f  where
  f s | Address (Just adr@(Addr wid i)) : val : rest <- stack s,
        Just (DataField cm) <- IntMap.lookup (unWordId wid) (variables s) =
          newState s { variables = IntMap.insert (unWordId wid) (DataField $ writeCell val adr cm)
                       (variables s),
                       stack = rest }
      | null (stack s) = emptyStack s
      | otherwise = abortWith "Bad arguments to !" s

-- | Given a counted string, extract the actual text as an individual ByteString.
countedText :: CV a -> FM a ByteString
countedText (Address (Just (Addr wid off))) = updateStateVal "" $ \s ->
    case IntMap.lookup (unWordId wid) (variables s) of
      Just (BufferField cmem) ->
          let count = fromIntegral $ B.index (chunk cmem) off
          in return (Right $ B.take count $ B.drop (off + 1) (chunk cmem), s)
      otherwise -> abortWith "expected address pointing to char buffer" s
countedText _ = abortMessage "expected address" >> return B.empty

xt word = XT (Just $ wordId word) (doer word)

-- | Find the name (counted string) in the dictionary
--   ( c-addr -- c-addr 0 | xt 1 | xt -1 )
find :: FM a ()
find = do
  caddr <- dpop
  mword <- searchDict =<< countedText caddr
  modify $ \s ->
      case mword of
        Just word
            | immediateFlag word -> s { stack = Val 1 : xt word : (stack s) }
            | otherwise -> s { stack = Val (-1) : xt word : (stack s) }
        Nothing -> s { stack = Val 0 : caddr : stack s }
  next

-- | Copy word from given address with delimiter to a special transient area.
--   ( "<chars>ccc<char>" -- c-addr )
xword :: FM a ()
xword = docol [inputLine, fetch, toIn, fetch, plus, parseName, toIn, plusStore, exit]
  where
    parseName =   -- ( "<spaces>ccc<space>" -- ctransbuf n )
      updateState  $ \s ->
         case stack s of
           Address (Just (Addr wid off)) : ss
             | Just (BufferField cmem) <- IntMap.lookup (unWordId wid) (variables s) ->
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
                 in newState s { stack = Val (fromIntegral inAdjust) : Address (Just $ Addr wordBufferWId 0) : ss,
                                 variables = IntMap.insert (unWordId wordBufferWId) countedField (variables s) }
           otherwise -> abortWith "parseName failed" s

-- | Interpreter - Compile given cell value
icompile adr@Address{} = tackOn $ WrapA $ lit adr
icompile val@Val{}     = tackOn $ WrapA $ lit val
icompile val@Text{}    = tackOn $ WrapA $ lit val
icompile (XT _ a)      = tackOn $ WrapA $ a

-- | Interpreter - Compile a cell value from the stack.
ilitComma x = tackOn $ WrapA $ lit x

-- | Compile a branch instruction. Branches need special handling when
--   the colon definition is finailized.
icompileBranch :: ([FM a ()] -> FM a ()) -> FM a ()
icompileBranch dest = updateState f  where
  f s | Just d <- defining s = newState s { defining = Just (tackOn (WrapB dest) d) }
      | otherwise = notDefining s

xcompileBranch _ = error "do not call xcompileBranch"

irecurse = tackOn WrapRecurse

tackOn x d = d { compileList = V.snoc (compileList d) x }


-- | Helper for create. Open up for defining a word assuming that the name of the
--   word can be found on top of stack.
--   ( caddr -- )  of word name to be created
create' :: ([FM a ()] -> FM a ()) -> Bool -> FM a ()
create' finalizer usingCreate = updateState f  where
  f s | Just{} <- defining s = abortWith "already compiling" s
      | Address (Just (Addr awid off)) : ss <- stack s,
        Just (BufferField cmem) <- IntMap.lookup (unWordId awid) (variables s) =
        let dict' = (dict s) { wids = wids' }
            wid : wids' = wids (dict s)
            linkhead = latest (dict s)
            name = B.drop (1 + off) $ chunk cmem
            (variables', code, cl)
              | usingCreate = (IntMap.insert (unWordId wid) (newDataField (target s) (unWordId wid) 0) (variables s), V.fromList (map WrapA [litAdr wid, exit]), closeDefining defining)
              | otherwise = (variables s, V.empty, id)
            defining = Defining code [] finalizer (ForthWord name False linkhead wid abort)
        in newState (cl (s { stack = ss,
                             dict = dict',
                             variables = variables',
                             defining = Just defining } ))
      | otherwise = abortWith "missing word name" s

-- | Smudge, terminate defining of a word and make it available.
smudge = updateState f  where
  f s | Just defining <- defining s = newState $ closeDefining defining s
      | otherwise = notDefining s

-- | Close the word being defined.
closeDefining :: Defining (FM a ()) -> FState a -> FState a
closeDefining defining s =
  let dict' = (dict s) { latest = Just word }
      word = (definingWord defining) { doer = (defineFinalizer defining) cs }
      vs = compileList defining
      -- Compile the branch instructions using the patch list provided by
      -- backpatch function. We rely on lazy evaluation here and insert
      -- branch destinations where lazy functions that will end up dropping
      -- 'dest' elements from final colon list.
      cs = map unWrap $ V.toList $ (V.//) vs (map f $ patchList defining)
      f (loc, dest) =
        let branchInstr | WrapB b <- (V.!) vs loc = WrapA $ b (Prelude.drop dest cs)
        in  (loc, branchInstr)
      unWrap WrapRecurse = branch cs
      unWrap (WrapA a) = a
  in s { defining = Nothing,
         dict = dict' }

here = updateState f  where
  f s | Just def <- defining s =
          let wid = wordId $ definingWord def
          in newState s { stack = HereColon wid (V.length (compileList def)) : stack s }
      | Just word <- latest (dict s), wid <- wordId word,
        Just (DataField mem) <- IntMap.lookup (unWordId wid) (variables s) =
          newState s { stack = Address (Just (Addr wid (dpOffset mem))) : stack s }
      | otherwise = abortWith "HERE only partially implemented" s

backpatch = updateState f  where
  f s | Just def <- defining s,
        HereColon _ loc : HereColon _ dest : ss <- stack s =
          let def' = def { patchList = (loc, dest) : patchList def }
          in newState s { defining = Just def',
                          stack = ss }
      | otherwise = notDefining s

backslash = docol body
  where body = toIn : fetch : inputLine : fetch : over : plus : inputLineLength : fetch : rot : minus : loop
        loop = lit (Val 1) : minus : dup : branch0 eol : over : cfetch : lit (Val 10) : minus : branch0 found : swap : lit (Val 1) : plus : swap : branch loop : eol
        eol = drop : drop : inputLineLength : fetch : toIn : store : exit : found
        found = [inputLineLength, fetch, swap, minus, toIn, store, drop, exit]

popFilename :: FM a String
popFilename =
  updateStateVal "" $ \s ->
    case stack s of
      Address (Just (Addr wid off)) : ss
        | Just (BufferField cmem) <- IntMap.lookup (unWordId wid) (variables s),
          not (B.null $ chunk cmem) ->
            let len = fromIntegral $ B.head $ chunk cmem
                name = C.take len $ C.drop (1 + off) $ chunk cmem
            in return (Right (C.unpack name), s { stack = ss })
        | otherwise -> abortWith "missing filename" s

loadSource = docol [xword, makeTempBuffer, evaluate, releaseTempBuffer, exit] where
  makeTempBuffer = do
    filename <- popFilename
    mc <- liftIO $ try $ readSourceFile filename
    case mc of
      Left (e :: IOException) -> abortMessage (show e)
      Right contents -> updateState $ \s ->
               let (handle, oldHandles', dict')
                     | null (oldHandles s) = let w:ws = (wids $ dict s)
                                             in (w, [], (dict s) { wids = ws })
                     | otherwise = (head $ oldHandles s, tail $ oldHandles s, dict s)
                   adr = Address (Just (Addr handle 0))
               in newState s { oldHandles = oldHandles',
                               dict = dict',
                               rstack = adr : rstack s,
                               stack = Val (fromIntegral $ C.length contents) : adr : stack s,
                               variables = IntMap.insert (unWordId handle) (textBuffer handle contents) (variables s) }

  releaseTempBuffer = updateState $ \s -> case rstack s of
                                            Address (Just (Addr handle 0)) : rs ->
                                              newState s { variables = IntMap.delete (unWordId handle) (variables s),
                                                           rstack = rs,
                                                           oldHandles = handle : oldHandles s }

-- | Generate code for a target
targetCodegen codeGenerate = docol [xword, dump, exit]
  where dump = do
          outputfile <- popFilename
          Just dict <- gets targetDict
          let text = codeGenerate dict
          mres <- liftIO $ try $ withFile outputfile WriteMode (flip L.hPut text)
          case mres of
            Left (e :: IOException) -> abortMessage $ show e
            Right () -> next

crossCompileSetup = updateState f  where
  f s = newState s { targetDict = Just (newDictionary extras)  }
  extras = return (WordId 0)

-- | Compile a string literal. We expect to get a string pointer (caddr u) on
--   the stack pointing to some character buffer. Compile a string literal
--   that has the execution semantics to push the string back on stack.
--   For the interpreter we simply wrap it in a literal.
compileString :: FM a ()
compileString = cprim . flip compile =<< liftM Text stringlit
  where stringlit = updateStateVal "" $ \s ->
                      case stack s of
                        Val n : Address (Just (Addr wid i)) : rest
                          | Just (BufferField bm) <- IntMap.lookup (unWordId wid) (variables s) ->
                              let text = B.take (fromIntegral n) (B.drop i (chunk bm))
                              in return (Right text, s { stack = rest } )
                          | otherwise -> abortWith "no text to compile" s
                        otherwise -> emptyStack s

-- | Make a string literal addressable on the fly.
addrString text s =
    case Map.lookup text (stringLiterals s) of
      Just addr -> (Address (Just addr), s)
      Nothing ->
          let (k:ks) = wids (dict s)
              addr = Addr k 0
              dict' = (dict s) { wids = ks }
              in (Address (Just addr), s { dict = dict',
                                           stringLiterals = Map.insert text addr
                                                            (stringLiterals s),
                                           variables = IntMap.insert (unWordId k) (textBuffer k text) (variables s) })

emit = dpop >>= emit1 >> next where
    emit1 (Val n) | n >= 0 = liftIO $ putStr [chr $ fromIntegral n]
    emit1 _ = liftIO $ putStr "?"

move :: FM a ()
move = do
  mtuple <- updateStateVal Nothing $ \s ->
    case stack s of
      Val count : Address (Just adrTo@(Addr widTo _iTo)) : Address (Just adrFrom@(Addr widFrom _iFrom)) : rest
          | Just (BufferField memTo) <- IntMap.lookup (unWordId widTo) (variables s),
            Just (BufferField memFrom) <- IntMap.lookup (unWordId widFrom) (variables s) ->
                return (Right (Just (count, adrFrom, memFrom, adrTo, memTo)), s { stack = rest })
      xs | length xs < 3 -> emptyStack s
         | otherwise -> abortWith "illegal arguments to MOVE" s
  case mtuple of
    Nothing -> return ()
    Just (count, adrFrom, memFrom, adrTo, memTo) ->
        liftIO $ blockMove (fromIntegral count) adrFrom memFrom adrTo memTo
  next

allot = updateState f  where
  f s | Val n:ss <- stack s =
        let Just word = latest (dict s)
            wid = wordId word
            Just (DataField mem) = IntMap.lookup (unWordId wid) (variables s)
            (offset, mem1) = updateDataPointer (fromIntegral n +) mem
        in newState s { variables = IntMap.insert (unWordId wid) (DataField mem1) (variables s),
                        stack = ss }
      | null (stack s) = emptyStack s
      | otherwise = abortWith "ALLOT requires integer value" s

umstar' = updateState f  where
  f s | n1@Val{} : n2@Val{} : ss <- stack s =
        let prod = unsigned n1 * unsigned n2
            Just bitsize = bitSizeMaybe n1
            low = mask prod
            high = mask $ prod `Bits.shiftR` bitsize
            mask x =  fromIntegral $ x Bits..&. ((1 `Bits.shiftL` bitsize) - 1)
        in newState s { stack = Val high : Val low : ss }
      | otherwise = abortWith "bad input to UM*" s

ummod' = updateState f  where
  f s | divisor@Val{} : hi@Val{} : lo@Val{} : ss <- stack s =
      let dividend = unsigned lo Bits..|. (unsigned hi `Bits.shiftL` bitsize)
          Just bitsize = bitSizeMaybe divisor
          (quot, rem) = dividend `quotRem` unsigned divisor
      in newState s { stack = Val (fromIntegral quot) : Val (fromIntegral rem) : ss }
    | otherwise = abortWith "bad input to UM/MOD" s

toBody = updateState f  where
  f s | XT (Just wid) a : ss <- stack s = newState s { stack = adrcv wid : ss }
      | otherwise = abortWith "bad input to >BODY" s

accept =
  let f s | Val n : caddr@Address{} : ss <- stack s =
              return (Right (fromIntegral n, caddr), s { stack = ss })
          | otherwise = abortWith "bad arguments to ACCEPT, or nu buffer destination" s
      g len s = newState s { stack = Val (fromIntegral len) : stack s }
  in do
    (n, caddr) <- updateStateVal (0, Address Nothing) f
    text <- liftM (C.take n) (liftIO C.getLine)
    copyTextBlock caddr text
    updateState (g $ C.length text)

-- Copy given chunk of text to given address. This will handle writing to
-- either cell memory and buffer memory.
copyTextBlock (Address (Just adrTo@(Addr wid _))) text =
  let f s = case IntMap.lookup (unWordId wid) (variables s) of
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
  f s | Just word <- latest (dict s),
        Just (DataField mem) <- IntMap.lookup (unWordId (wordId word)) (variables s) =
          let mem' = DataField $ alignDP mem (target s)
          in newState s { variables = IntMap.insert (unWordId (wordId word)) mem' (variables s) }
      | otherwise = abortWith "cannot ALIGN, dp no valid" s

aligned = updateState f  where
  f s | Address (Just (Addr wid off)) : ss <- stack s =
          newState s { stack = Address (Just (Addr wid (alignOffset off (target s)))) : ss }
      | otherwise = abortWith "ALIGNED only words on addresses" s

depth = updateState f  where
  f s = newState s { stack = Val (fromIntegral $ length $ stack s) : stack s }

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
    updateState $ \s -> newState s { stack = Val (fromIntegral $ ord c) : stack s }
