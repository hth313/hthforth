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
import Prelude hiding (drop)
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
          mapM_ f [(sourceWId, 0), (stateWId, 0), (toInWId, 0),
                   (inputLineWId, 0), (inputLineLengthWId, 0),
                   (sourceIDWid, 0)]
          mapM_ g [(tregWid, 100)]

interpreterDictionary :: Cell cell => Dictionary (FM cell ())
interpreterDictionary = newDictionary extras
  where extras = do
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
          addWord "EMIT" emit
          addWord "MOVE" move
          addWord "FIND" find
          addWord "TREG" treg
          addWord "STRING," compileString
          addWord "LIT," litComma

-- | Foundation of the Forth interpreter
instance Cell cell => Primitive (CV cell) (FM cell ()) where
  semi = rpop >>= \case
           IP ip' -> do
             modify $ \s -> s { ip = ip' }
             next
           otherwise -> abortMessage "IP not on rstack"
  execute = call =<< dpop
  evaluate = evaluate'
  lit (Text text) = modify (\s ->
                       let u = fromIntegral $ C.length text
                           (caddr, s') = addrString text s
                       in  s' { stack = Val u : caddr : stack s' }) >> next
  lit val = dpush val >> next
  false = lit falseVal
  true = lit trueVal
  swap = updateState $ \s -> case stack s of
                               s0 : s1 : ss -> newState s { stack = s1 : s0 : ss }
                               otherwise -> emptyStack s
  drop = updateState $ \s -> case stack s of
                               _ : ss -> newState s { stack = ss }
                               otherwise -> emptyStack s
  dup = updateState $ \s -> case stack s of
                              ss@(s0 : _) -> newState s { stack = s0 : ss }
                              otherwise -> emptyStack s
  over = updateState $ \s -> case stack s of
                               ss@(_ : s1 : _) -> newState s { stack = s1 : ss }
                               otherwise -> emptyStack s
  rot = updateState $ \s -> case stack s of
                              s0 : s1 : s2 : ss -> newState s { stack = s2 : s0 : s1 : ss }
                              otherwise -> emptyStack s
  tor = updateState $ \s -> case stack s of
                              s0 : ss -> newState s { stack = ss, rstack = s0 : rstack s }
                              otherwise -> emptyStack s
  rto = updateState $ \s -> case rstack s of
                              r0 : rs -> newState s { rstack = rs, stack = r0 : stack s }
                              otherwise -> emptyStack s
  rfetch = updateState $ \s -> case rstack s of
                                 r0 : _ -> newState s { stack = r0 : stack s }
                                 otherwise -> emptyStack s
  cfetch = cfetch'
  fetch = fetch'
  cstore = cstore'
  store = store'
  plus  = binary (+)
  minus = binary (-)
  star  = binary (*)
  slash = binary divide
  and   = binary (Bits..&.)
  or    = binary (Bits..|.)
  xor   = binary Bits.xor

  zerop = updateState $ \s -> case stack s of
                                (Val 0) : ss         -> newState s { stack = trueVal  : ss }
                                Val{} : ss           -> newState s { stack = falseVal : ss }
                                Address Nothing : ss -> newState s { stack = trueVal : ss }
                                Address{} : ss       -> newState s { stack = falseVal : ss }
                                otherwise            -> emptyStack s
  lt0 = updateState $ \s -> case stack s of
                              (Val n) : ss -> let flag | n < 0 = trueVal
                                                       | otherwise = falseVal
                                              in newState s { stack = flag : ss }
                              otherwise -> emptyStack s
  docol xs = modify (\s -> s { rstack = IP (ip s) : rstack s, ip = xs }) >> next
  branch = ipdo
  branch0 loc = dpop >>= \n -> if | isZero n -> ipdo loc
                                  | otherwise  -> next
  -- variables
  state           = litAdr stateWId
  sourceId        = litAdr sourceWId
  toIn            = litAdr toInWId
  inputBuffer     = litAdr inputBufferWId
  inputLine       = litAdr inputLineWId
  inputLineLength = litAdr inputLineLengthWId
  sourceID        = litAdr sourceIDWid

  -- Compiling words
  constant = docol [xword, create' head False, compileComma, smudge, semi]

-- Forward declarations of Forth words implemented by the interpreter
xif, xelse, xthen, xdo, loop, plusLoop, leave, quit :: Cell cell => FM cell ()
interpret, plusStore, create, does, colon, semicolon :: Cell cell => FM cell ()
compileComma, comma, smudge, immediate, pdo, ploop, pplusLoop :: Cell cell => FM cell ()
here, backpatch, backslash, loadSource, emit, treg, litComma :: Cell cell => FM cell ()

treg = litAdr tregWid

-- Control structures
xif   = docol [here, compileBranch branch0, semi]
xelse = docol [here, compileBranch branch, here, rot, backpatch, semi]
xthen = docol [here, swap, backpatch, semi]

xdo = docol [compile (XT pdo), here, semi]
loop = xloop ploop
plusLoop = xloop pplusLoop
leave = updateState $ \s -> case rstack s of
                              _ : rs@(limit : _) -> newState s { rstack = limit : rs }
                              otherwise -> emptyStack s

quit = ipdo [ (modify (\s -> s { rstack = [], stack = Val 0 : stack s }) >> next),
              sourceId, store, mainLoop ]

plusStore = docol [dup, fetch, rot, plus, swap, store, semi]

create = docol [xword, create' docol True, semi]
colon = docol [lit (Val (-1)), state, store, xword, create' docol False, semi]
semicolon = docol [compile (XT semi), lit (Val 0), state, store, smudge, semi]
compileComma = dpop >>= compile
immediate = updateState $ \s -> newState s { dict = setLatestImmediate (dict s) }

comma = updateState $ \s ->
  case stack s of
    c:ss ->
      let Just word = latest (dict s)
          wid = wordId word
          Just (DataField mem) = IntMap.lookup (unWordId wid) (variables s)
          (offset, mem1) = updateDataPointer (bytesPerCell (target s) +) mem
          adr = Addr wid offset
          mem2 = writeCell c adr mem1
      in newState s { variables = IntMap.insert (unWordId wid) (DataField mem2) (variables s),
                      stack = ss }
    otherwise -> emptyStack s

does = updateState $ \s ->
  case rstack s of
    IP ip' : rs ->
      let Just word = latest (dict s)
          word' = word { doer = docol (litAdr (wordId word) : ip s) }
          dict' = (dict s) { latest = Just word' }
      in newState s { ip = ip',
                      dict = dict',
                      rstack = rs }
    [] -> emptyStack s
    otherwise -> abortWith "IP not on rstack" s

-- Helper function that compile the ending loop word
xloop a = docol [compile (XT a), here, compileBranch branch0, backpatch, semi]

-- Runtime words for DO-LOOPs
pdo = updateState $ \s -> case stack s of
                            s0 : s1 : ss -> newState s { stack = ss,
                                                         rstack  = s0 : s1 : rstack s }
                            otherwise -> emptyStack s
ploop = updateState $ rloopHelper (Val 1 +)
pplusLoop = updateState $ \s -> case stack s of
                                  n : ss -> rloopHelper (n+) (s { stack = ss })
rloopHelper f s = case rstack s of
                    i : r2@(limit : rs) ->
                        let i' = f i
                        in newState $ if i' < limit
                                      then s { rstack = i' : r2,
                                               stack = falseVal : stack s }
                                      else s { rstack = rs,
                                               stack = trueVal : stack s }
                    otherwise -> emptyStack s

binary op = updateState $ \s -> case stack s of
                                  op1 : op2 : ss -> newState s { stack = op2 `op` op1 : ss }
                                  otherwise -> emptyStack s

divide :: Cell cell => CV cell -> CV cell -> CV cell
divide (Val a) (Val b) = Val (a `div` b)
divide a b = Bot $ "non numeric divide"

ipdo :: Cell cell => [FM cell ()] -> FM cell ()
ipdo ip' = modify (\s -> s { ip = ip' }) >> next

searchDict :: Cell cell => ByteString -> FM cell (Maybe (ForthWord (FM cell ())))
searchDict n = gets (f . latest . dict)
  where f jw@(Just word) | n == name word = jw
                         | otherwise = f (link word)
        f Nothing = Nothing

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
          parse bs = case readDec text of
                       [(x,"")] -> lit $ Val x
                       otherwise -> abortMessage $ text ++ " ?"
                       where text = C.unpack bs

evaluate' :: Cell cell => FM cell ()
evaluate' = docol [inputLine, fetch, tor,              -- save input specification
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
litAdr wid = lit $ Address (Just $ Addr wid 0)

abort :: Cell cell => FM cell ()
abort = docol [modify (\s -> s { stack = [], defining = Nothing }) >> next,
               lit (Val 0), state, store, quit]

abort0 :: Cell cell => FM cell (CV cell)
abort0 = abort >> return (Val 0)

next :: Cell cell => FM cell ()
next = do x <- StateT $ \s -> let (x:xs) = ip s
                              in return (x, s { ip = xs } )
          x

call :: Cell cell => CV cell -> FM cell ()
call (XT a) = a
call _ = abortMessage "not an execution token"

-- Data stack primitives
dpush :: CV cell -> FM cell ()
dpush val = modify $ \s -> s { stack = val : stack s }

dpop :: Cell cell => FM cell (CV cell)
dpop = updateStateVal (Val 0) $ \s ->
         case stack s of
           t:ts -> return (Right t, s { stack = ts })
           [] -> emptyStack s

rpop :: Cell cell => FM cell (CV cell)
rpop = updateStateVal (Val 0) $ \s ->
         case rstack s of
           t:ts -> return (Right t, s { rstack = ts })
           [] -> emptyStack s

updateState f = StateT f >>= \case
                  Left msg -> abortMessage msg
                  Right () -> next

updateStateVal x f = StateT f >>= \case
                        Left msg -> abortMessage msg >> return x
                        Right y -> return y

newState s = return (Right (), s)

emptyStack = abortWith "empty stack"
notDefining = abortWith "not defining"
abortWith msg s = return (Left msg, s)
abortMessage msg = liftIO (putStrLn msg) >> abort

cfetch' :: Cell cell => FM cell ()
cfetch' = updateState $ \s ->
    case stack s of
      Address (Just adr@(Addr wid _)) : rest ->
        case IntMap.lookup (unWordId wid) (variables s) of
          Just (BufferField buf) ->
              let c = Val $ fromIntegral $ read8 adr buf
              in  newState s { stack = c : rest }
          Nothing -> abortWith "C@ - no valid address" s
          Just DataField{} -> abortWith "C@ - data field not implemented" s
      [] -> emptyStack s
      x -> abortWith "bad C@ address" s

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
fetch' = updateState $ \s ->
    case stack s of
      Address (Just adr@(Addr wid _)) : rest ->
          case IntMap.lookup (unWordId wid) (variables s) of
            Just (DataField cm) ->
                case readCell adr cm of
                  Just x -> newState s { stack = x : rest }
                  otherwise -> abortWith "@ outside data field" s
            Just (BufferField mem) -> abortWith "@ in buffer field" s
            Nothing -> abortWith "no data field" s
      [] -> emptyStack s
      a : _ -> abortWith "bad address given to @" s

store' :: Cell cell => FM cell ()
store' = updateState $ \s ->
    case stack s of
      Address (Just adr@(Addr wid i)) : val : rest
          | Just (DataField cm) <- IntMap.lookup (unWordId wid) (variables s) ->
              newState s { variables = IntMap.insert (unWordId wid) (DataField $ writeCell val adr cm)
                                       (variables s),
                           stack = rest }
      [] -> emptyStack s
      otherwise -> abortWith "Bad arguments to !" s

-- | Given a counted string, extract the actual text as an individual ByteString.
countedText :: Cell cell => CV cell -> FM cell ByteString
countedText (Address (Just (Addr wid off))) = updateStateVal "" $ \s ->
    case IntMap.lookup (unWordId wid) (variables s) of
      Just (BufferField cmem) ->
          let count = fromIntegral $ B.index (chunk cmem) off
          in return (Right $ B.take count $ B.drop (off + 1) (chunk cmem), s)
      otherwise -> abortWith "expected address pointing to char buffer" s
countedText _ = abortMessage "expected address" >> return B.empty

-- | Find the name (counted string) in the dictionary
--   ( c-addr -- c-addr 0 | xt 1 | xt -1 )
find :: Cell cell => FM cell ()
find = do
  caddr <- dpop
  mword <- searchDict =<< countedText caddr
  modify $ \s ->
      case mword of
        Just word
            | immediateFlag word -> s { stack = Val 1 : XT (doer word) : (stack s) }
            | otherwise -> s { stack = Val (-1) : XT (doer word) : (stack s) }
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

compile :: Cell cell => CV cell -> FM cell ()
compile adr@Address{} = tackOn $ WrapA $ lit adr
compile val@Val{} = tackOn $ WrapA $ lit val
compile val@Text{} = tackOn $ WrapA $ lit val
compile (XT a) = tackOn $ WrapA $ a

litComma = dpop >>= tackOn . WrapA . lit

compileBranch :: Cell cell => ([FM cell ()] -> FM cell ()) -> FM cell ()
compileBranch = tackOn . WrapB

tackOn x = updateState $ \s ->
  case defining s of
    Nothing -> notDefining s
    Just d -> newState s { defining = Just (d { compileList = V.snoc (compileList d) x } ) }

-- Helper for create. Open up for defining a word assuming that the name of the
-- word can be found on top of stack.
-- ( caddr -- )  of word name to be created
create' :: Cell cell => ([FM cell ()] -> FM cell ()) -> Bool -> FM cell ()
create' finalizer usingCreate = updateState $ \s ->
  case defining s of
    Just{}  -> abortWith "already compiling" s
    Nothing -> case stack s of
                 Address (Just (Addr awid off)) : ss
                   | Just (BufferField cmem) <- IntMap.lookup (unWordId awid) (variables s) ->
                       let dict' = (dict s) { wids = wids' }
                           wid : wids' = wids (dict s)
                           linkhead = latest (dict s)
                           name = B.drop (1 + off) $ chunk cmem
                           (variables', code, cl)
                             | usingCreate = (IntMap.insert (unWordId wid) (newDataField (target s) (unWordId wid) 0) (variables s), V.fromList (map WrapA [litAdr wid, semi]), closeDefining defining)
                             | otherwise = (variables s, V.empty, id)
                           defining = Defining code [] finalizer
                                        (ForthWord name False linkhead wid abort)
                       in newState (cl (s { stack = ss,
                                            dict = dict',
                                            variables = variables',
                                            defining = Just defining } ))
                 otherwise -> abortWith "missing word name" s

-- Helper for smudge, terminate defining of a word and make it available.
smudge = updateState $ \s ->
  case defining s of
    Nothing -> notDefining s
    Just defining  -> newState $ closeDefining defining s

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

here = updateState $ \s ->
         case defining s of
           Nothing  -> abortWith "HERE only partially implemented" s
           Just def -> newState s { stack = HereColon wid (V.length (compileList def)) : stack s }
             where wid = wordId $ definingWord def

backpatch = updateState $ \s ->
         case defining s of
           Nothing  -> notDefining s
           Just def -> case stack s of
                         HereColon _ loc : HereColon _ dest : ss ->
                           let def' = def { patchList = (loc, dest) : patchList def }
                           in newState s { defining = Just def',
                                           stack = ss }

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
    emit1 (Val n) = liftIO $ putStr [chr $ fromIntegral n]
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
