{-# LANGUAGE FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternGuards, ScopedTypeVariables #-}
{- |

   The Forth interpreter.

-}

module Language.Forth.Interpreter (initialState, initialVarStorage, quit) where

import Numeric
import Control.Exception (try)
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
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import Language.Forth.Interpreter.Address
import Language.Forth.Interpreter.CellMemory
import Language.Forth.Interpreter.DataField
import Language.Forth.Cell
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Interpreter.Monad
import Language.Forth.Primitive
import Language.Forth.StreamFile
import Language.Forth.Target
import Language.Forth.Word
import Util.Memory
import Prelude hiding (drop, until)
import qualified Prelude as Prelude

initialState :: Cell cell => Target cell -> FState cell
initialState target = FState [] [] [] target interpreterDictionary IntMap.empty [] Map.empty Nothing

initialVarStorage :: Cell cell => FM cell ()
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

interpreterDictionary :: Cell cell => Dictionary (FM cell ())
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
          addWord "," comma
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
          addWord "EMIT" emit
          addWord "MOVE" move
          addWord "FIND" find
          addWord "TREG" treg
          addWord "PAD" pad
          addWord "STRING," compileString
          addWord "LIT," litComma
          addWord "ALLOT" allot
          addWord ">BODY" toBody

-- | Foundation of the Forth interpreter
instance Cell cell => Primitive (CV cell) (FM cell ()) where
  semi = rpop >>= \case
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

  twoStar = docol [dup, plus, semi]
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
  docol xs = modify (\s -> s { rstack = IP (ip s) : rstack s, ip = xs }) >> next
  branch = ipdo
  branch0 loc = dpop >>= \n -> if | isZero n -> ipdo loc
                                  | otherwise  -> next
  constant = docol [xword, create' head False, compileComma, smudge, semi]

  umstar = umstar'
  ummod = ummod'

-- Forward declarations of Forth words implemented by the interpreter
xif, xelse, xthen, xdo, loop, plusLoop, leave, begin, until, again :: Cell cell => FM cell ()
interpret, plusStore, create, does, colon, semicolon, quit :: Cell cell => FM cell ()
compileComma, comma, smudge, immediate, pdo, ploop, pplusLoop :: Cell cell => FM cell ()
here, backpatch, backslash, loadSource, emit, treg, pad, litComma :: Cell cell => FM cell ()
allot, umstar', ummod', rot, evaluate, false, true :: Cell cell => FM cell ()
state, sourceID, toIn, inputBuffer, inputLine, inputLineLength :: Cell cell => FM cell ()
toBody :: Cell cell => FM cell ()

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
pad = docol [treg, lit (Val 64), plus, semi]

-- Control structures
xif   = docol [here, compileBranch branch0, semi]
xelse = docol [here, compileBranch branch, here, rot, backpatch, semi]
xthen = docol [here, swap, backpatch, semi]

xdo = docol [compile (XT Nothing pdo), here, semi]
loop = xloop ploop
plusLoop = xloop pplusLoop
leave = updateState f  where
  f s | _ : rs@(limit : _) <- rstack s = newState s { rstack = limit : rs }
      | otherwise = emptyStack s
begin = here
until = docol [here, compileBranch branch0, backpatch, semi]
again = docol [here, compileBranch branch, backpatch, semi]

quit = ipdo [ (modify (\s -> s { rstack = [], stack = Val 0 : stack s }) >> next),
              sourceID, store, mainLoop ]

plusStore = docol [dup, fetch, rot, plus, swap, store, semi]

create = docol [xword, create' docol True, semi]
colon = docol [lit (Val (-1)), state, store, xword, create' docol False, semi]
semicolon = docol [compile (XT Nothing semi), lit (Val 0), state, store, smudge, semi]
compileComma = dpop >>= compile
immediate = updateState $ \s -> newState s { dict = setLatestImmediate (dict s) }

comma = updateState f  where
  f s | c:ss <- stack s =
      let Just word = latest (dict s)
          wid = wordId word
          Just (DataField mem) = IntMap.lookup (unWordId wid) (variables s)
          (offset, mem1) = updateDataPointer (bytesPerCell (target s) +) mem
          adr = Addr wid offset
          mem2 = writeCell c adr mem1
      in newState s { variables = IntMap.insert (unWordId wid) (DataField mem2) (variables s),
                      stack = ss }
      | otherwise = emptyStack s

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
xloop a = docol [compile (XT Nothing a), here, compileBranch branch0, backpatch, semi]

-- | Runtime words for DO-LOOPs
pdo = updateState f  where
  f s | s0 : s1 : ss <- stack s = newState s { stack = ss,
                                               rstack  = s0 : s1 : rstack s }
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
binary :: Cell cell => (CV cell -> CV cell -> CV cell) -> FM cell ()
binary op = updateState f  where
  f s | op1 : op2 : ss <- stack s = newState s { stack = op2 `op` op1 : ss }
      | otherwise = emptyStack s

-- | Convert a cell value to a large unsigned number
unsigned :: Cell cell => CV cell -> Word64
unsigned c@(Val x) =
  let (ux :: Word64) = fromIntegral x
      Just bitsize = Bits.bitSizeMaybe c
      bitmask = (1 `Bits.shiftL` bitsize) - 1
  in ux Bits..&. bitmask

-- | Replace what we are interpreting with given slice of code.
--   Typically used for implementing branches and setting the
--   main loop.
ipdo :: Cell cell => [FM cell ()] -> FM cell ()
ipdo ip' = modify (\s -> s { ip = ip' }) >> next

-- | Search dictionary for given named word.
searchDict :: Cell cell => ByteString -> FM cell (Maybe (ForthWord (FM cell ())))
searchDict n = gets (f . latest . dict)
  where f jw@(Just word) | n == name word = jw
                         | otherwise = f (link word)
        f Nothing = Nothing

-- | Main loop for the interpreter
mainLoop :: Cell cell => FM cell ()
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
  where begin = xword : dup : cfetch : zerop : branch0 lab1 : drop : semi : lab1
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
                  rto, inputLine, store, semi]

-- | Insert the field contents of given word
putField :: Cell cell => WordId -> DataField cell (FM cell ()) -> FM cell ()
putField wid field = modify $ \s -> s { variables = IntMap.insert (unWordId wid) field  (variables s) }

-- | Push the field address of a word on stack
litAdr :: Cell cell => WordId -> FM cell ()
litAdr = lit . adrcv

-- | Addressable value, pointing to the first address of the datafield of
--   given word.
adrcv wid = Address (Just $ Addr wid 0)

-- | Forth level error handling.
abort :: Cell cell => FM cell ()
abort = docol [modify (\s -> s { stack = [], defining = Nothing }) >> next,
               lit (Val 0), state, store, quit]

emptyStack = abortWith "empty stack"
notDefining = abortWith "not defining"
abortWith msg s = return (Left msg, s)
abortMessage msg = liftIO (putStrLn msg) >> abort

-- | Step the colon body and execute next word in it.
next :: Cell cell => FM cell ()
next = do x <- StateT $ \s -> let (x:xs) = ip s
                              in return (x, s { ip = xs } )
          x

-- | Invoke an execution token.
call :: Cell cell => CV cell -> FM cell ()
call (XT _ a) = a
call _ = abortMessage "not an execution token"

-- | Data stack primitives
dpush :: CV cell -> FM cell ()
dpush val = modify $ \s -> s { stack = val : stack s }

dpop :: Cell cell => FM cell (CV cell)
dpop = updateStateVal (Val 0) f  where
  f s | t:ts <- stack s = return (Right t, s { stack = ts })
      | otherwise = emptyStack s

-- | Return stack primitives
rpop :: Cell cell => FM cell (CV cell)
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

cfetch' :: Cell cell => FM cell ()
cfetch' = updateState f  where
  f s | Address (Just adr@(Addr wid _)) : rest <- stack s =
          case IntMap.lookup (unWordId wid) (variables s) of
            Just (BufferField buf) ->
              let c = Val $ fromIntegral $ read8 adr buf
              in  newState s { stack = c : rest }
            Nothing -> abortWith "C@ - no valid address" s
            Just DataField{} -> abortWith "C@ - data field not implemented" s
      | null (stack s) = emptyStack s
      | otherwise = abortWith "bad C@ address" s

cstore' :: Cell cell => FM cell ()
cstore' = do
  action <- updateStateVal (return ()) $ \s ->
    case stack s of
      Address (Just adr@(Addr wid i)) : Val val : rest
          | Just (BufferField bm) <- IntMap.lookup (unWordId wid) (variables s) ->
              return (Right (write8 (fromIntegral val) adr bm), s { stack = rest })
      [] -> emptyStack s
      [x] -> abortWith "no value to C! to" s
      x:_ -> abortWith "cannot C! to non-address" s
  liftIO action
  next

fetch' :: Cell cell => FM cell ()
fetch' = updateState f  where
  f s | Address (Just adr@(Addr wid _)) : rest <- stack s =
          case IntMap.lookup (unWordId wid) (variables s) of
            Just (DataField cm) ->
                case readCell adr cm of
                  Just x -> newState s { stack = x : rest }
                  otherwise -> abortWith "@ outside data field" s
            Just (BufferField mem) -> abortWith "@ in buffer field" s
            Nothing -> abortWith "no data field" s
      | null (stack s) = emptyStack s
      | otherwise = abortWith "bad address given to @" s

store' :: Cell cell => FM cell ()
store' = updateState f  where
  f s | Address (Just adr@(Addr wid i)) : val : rest <- stack s,
        Just (DataField cm) <- IntMap.lookup (unWordId wid) (variables s) =
          newState s { variables = IntMap.insert (unWordId wid) (DataField $ writeCell val adr cm)
                       (variables s),
                       stack = rest }
      | null (stack s) = emptyStack s
      | otherwise = abortWith "Bad arguments to !" s

-- | Given a counted string, extract the actual text as an individual ByteString.
countedText :: Cell cell => CV cell -> FM cell ByteString
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
find :: Cell cell => FM cell ()
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
xword :: Cell cell => FM cell ()
xword = docol [inputLine, fetch, toIn, fetch, plus, parseName, toIn, plusStore, semi]
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

-- | Compile given cell value
compile :: Cell cell => CV cell -> FM cell ()
compile adr@Address{} = tackOn $ WrapA $ lit adr
compile val@Val{}     = tackOn $ WrapA $ lit val
compile val@Text{}    = tackOn $ WrapA $ lit val
compile (XT _ a)      = tackOn $ WrapA $ a

-- | Compile a cell value from the stack.
litComma = dpop >>= tackOn . WrapA . lit

-- | Compile a branch instruction. Branches need special handling when
--   the colon definition is finailized.
compileBranch :: Cell cell => ([FM cell ()] -> FM cell ()) -> FM cell ()
compileBranch = tackOn . WrapB

tackOn x = updateState f  where
  f s | Just d <- defining s =
          newState s { defining = Just (d { compileList = V.snoc (compileList d) x } ) }
      | otherwise = notDefining s

-- | Helper for create. Open up for defining a word assuming that the name of the
--   word can be found on top of stack.
--   ( caddr -- )  of word name to be created
create' :: Cell cell => ([FM cell ()] -> FM cell ()) -> Bool -> FM cell ()
create' finalizer usingCreate = updateState f  where
  f s | Just{} <- defining s = abortWith "already compiling" s
      | Address (Just (Addr awid off)) : ss <- stack s,
        Just (BufferField cmem) <- IntMap.lookup (unWordId awid) (variables s) =
        let dict' = (dict s) { wids = wids' }
            wid : wids' = wids (dict s)
            linkhead = latest (dict s)
            name = B.drop (1 + off) $ chunk cmem
            (variables', code, cl)
              | usingCreate = (IntMap.insert (unWordId wid) (newDataField (target s) (unWordId wid) 0) (variables s), V.fromList (map WrapA [litAdr wid, semi]), closeDefining defining)
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
closeDefining :: Cell cell => Defining cell -> FState cell -> FState cell
closeDefining defining s =
  let dict' = (dict s) { latest = Just word }
      word = (definingWord defining) { doer = (defineFinalizer defining) cs }
      vs = compileList defining
      -- Compile the branch instructions using the patch list provided by
      -- backpatch function. We rely on lazy evaluation here and insert
      -- branch destinations where lazy functions that will end up dropping
      -- 'dest' elements from final colon list.
      cs = map unWrapA $ V.toList $ (V.//) vs (map f $ patchList defining)
      f (loc, dest) =
        let branchInstr | WrapB b <- (V.!) vs loc = WrapA $ b (Prelude.drop dest cs)
        in  (loc, branchInstr)
  in s { defining = Nothing,
         dict = dict' }

here = updateState f  where
  f s | Just def <- defining s =
          let wid = wordId $ definingWord def
          in newState s { stack = HereColon wid (V.length (compileList def)) : stack s }
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
        eol = drop : drop : inputLineLength : fetch : toIn : store : semi : found
        found = [inputLineLength, fetch, swap, minus, toIn, store, drop, semi]

loadSource = docol [xword, makeTempBuffer, evaluate, releaseTempBuffer, semi] where
  makeTempBuffer = do
    filename <- updateStateVal "" (
                  \s -> case stack s of
                          Address (Just (Addr wid off)) : ss
                            | Just (BufferField cmem) <- IntMap.lookup (unWordId wid) (variables s),
                              not (B.null $ chunk cmem) ->
                                let len = fromIntegral $ B.head $ chunk cmem
                                    name = C.take len $ C.drop (1 + off) $ chunk cmem
                                in return (Right (C.unpack name), s { stack = ss })
                            | otherwise -> abortWith "missing filename" s)
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

-- | Compile a string literal. We expect to get a string pointer (caddr u) on
--   the stack pointing to some character buffer. Compile a string literal
--   that has the execution semantics to push the string back on stack.
--   For the interpreter we simply wrap it in a literal.
compileString :: Cell cell => FM cell ()
compileString = compile =<< liftM Text stringlit
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

move :: Cell cell => FM cell ()
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
            Just bitsize = Bits.bitSizeMaybe n1
            low = mask prod
            high = mask $ prod `Bits.shiftR` bitsize
            mask x =  fromIntegral $ x Bits..&. ((1 `Bits.shiftL` bitsize) - 1)
        in newState s { stack = Val high : Val low : ss }
      | otherwise = abortWith "bad imput to UM*" s

ummod' = updateState f  where
  f s | divisor@Val{} : hi@Val{} : lo@Val{} : ss <- stack s =
      let dividend = unsigned lo Bits..|. (unsigned hi `Bits.shiftL` bitsize)
          Just bitsize = Bits.bitSizeMaybe divisor
          (quot, rem) = dividend `divMod` unsigned divisor
      in newState s { stack = Val (fromIntegral quot) : Val (fromIntegral rem) : ss }
    | otherwise = abortWith "bad input to UM/MOD" s

toBody = updateState f  where
  f s | XT (Just wid) a : ss <- stack s = newState s { stack = adrcv wid : ss }
      | otherwise = abortWith "bad input to >BODY" s
