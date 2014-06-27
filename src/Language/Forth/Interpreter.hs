{-# LANGUAGE FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE MultiWayIf, OverloadedStrings, PatternGuards #-}
{- |

   The Forth interpreter.

-}

module Language.Forth.Interpreter (initialState, initialVarStorage, quit) where

import Numeric
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import qualified Data.Bits as Bits
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
import Language.Forth.Target
import Language.Forth.Word
import Util.Memory
import Prelude hiding (drop)

initialState :: Cell cell => Target cell -> FState cell
initialState target = FState [] [] [] target interpreterDictionary IntMap.empty Nothing

initialVarStorage :: Cell cell => FM cell ()
initialVarStorage = gets target >>=
  \t -> let f (wid, val) =
              let field@(DataField cm) = newDataField t wid (bytesPerCell t)
              in  putField wid (DataField $ writeCell val (Addr wid 0) cm)
        in mapM_ f [(sourceWId, 0), (stateWId, 0), (toInWId, 0),
                    (inputLineWId, 0), (inputLineLengthWId, 0),
                    (sourceIDWid, 0)]

interpreterDictionary :: Cell cell => Dictionary (FM cell ())
interpreterDictionary = newDictionary extras
  where extras = do
          addWord "\\"   backslash >> makeImmediate
          addWord "BYE" (liftIO exitSuccess)

-- | Foundation of the Forth interpreter
instance Cell cell => Primitive (CV cell) (FM cell ()) where
  semi = rpop >>= \case
           IP ip' -> do
             modify $ \s -> s { ip = ip' }
             next
           otherwise -> abortMessage "IP not on rstack"
  execute = call =<< dpop
  evaluate = evaluate'
  lit val = dpush val >> next
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
  store = store'
  plusStore = docol [dup, fetch, rot, plus, swap, store, semi]
  plus  = binary (+)
  minus = binary (-)
  star  = binary (*)
  slash = binary divide
  and   = binary (Bits..&.)
  or    = binary (Bits..|.)
  xor   = binary Bits.xor

  zerop = updateState $ \s -> case stack s of
                                (Val 0) : ss         -> newState s { stack = true  : ss }
                                Val{} : ss           -> newState s { stack = false : ss }
                                Address Nothing : ss -> newState s { stack = true : ss }
                                Address{} : ss       -> newState s { stack = false : ss }
                                otherwise            -> emptyStack s
  quit = ipdo [ (modify (\s -> s { rstack = [], stack = Val 0 : stack s }) >> next),
                sourceId, store, mainLoop ]
  interpret = interpret'
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
  create = docol [word, create', semi]
  colon = docol [lit (Val (-1)), state, store, create, semi]
  semicolon = docol [compile (XT semi), lit (Val 0), state, store, smudge, semi]
  compileComma = dpop >>= compile
  smudge = smudge'

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

interpret' :: Cell cell => FM cell ()
interpret' = docol begin
  where begin = word : dup : cfetch : zerop : branch0 lab1 : drop : semi : lab1
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
            | immediate word -> s { stack = Val 1 : XT (doer word) : (stack s) }
            | otherwise -> s { stack = Val (-1) : XT (doer word) : (stack s) }
        Nothing -> s { stack = Val 0 : caddr : stack s }
  next

-- | Copy word from given address with delimiter to a special transient area.
--   ( "<chars>ccc<char>" -- c-addr )
word :: Cell cell => FM cell ()
word = docol [inputLine, fetch, toIn, fetch, plus, parseName, toIn, plusStore, semi]
  where
    parseName =   -- ( "<spaces>ccc<space>" -- ctransbuf n )
      updateState  $ \s ->
         case stack s of
           Address (Just (Addr wid off)) : ss
             | Just (BufferField cmem) <- IntMap.lookup (unWordId wid) (variables s) ->
                 let start = B.drop off (chunk cmem)
                     (skipCount, nameStart) = skipSpaces start
                     skipSpaces bs
                       | B.null bs = (0, bs)
                       | otherwise = skipSpaces1 0 bs where
                     skipSpaces1 n bs
                       | C.head bs <= ' ' = skipSpaces1 (n + 1) (B.tail bs)
                       | otherwise = (n, bs)
                     name = C.takeWhile (> ' ') nameStart
                     nameLength = C.length name
                     inAdjust = skipCount + nameLength
                     countedField = textBuffer wordBufferWId (B.cons (fromIntegral nameLength) name)
                 in newState s { stack = Val (fromIntegral inAdjust) : Address (Just $ Addr wordBufferWId 0) : ss,
                                 variables = IntMap.insert (unWordId wordBufferWId) countedField (variables s) }
           otherwise -> abortWith "parseName failed" s

compile :: Cell cell => CV cell -> FM cell ()
compile cv = updateState $ \s ->
  case defining s of
    Nothing -> abortWith "not defining" s
    Just d  -> case cv of
                 adr@Address{} -> tack $ lit adr
                 val@Val{}     -> tack $ lit val
                 XT a          -> tack a
      where tack a = newState s { defining = Just (d { compileList = V.snoc ( compileList d) a } ) }


-- Helper for create. Open up for defining a word assuming that the name of the
-- word can be found on top of stack.
-- ( caddr -- )  of word name to be created
create' :: Cell cell => FM cell ()
create' = updateState $ \s ->
  case defining s of
    Just{}  -> abortWith "already compiling" s
    Nothing -> case stack s of
                 Address (Just (Addr awid off)) : ss
                   | Just (BufferField cmem) <- IntMap.lookup (unWordId awid) (variables s) ->
                       let dict' = (dict s) { wids = wids' }
                           wid : wids' = wids (dict s)
                           linkhead = latest (dict s)
                           name = B.drop (1 + off) $ chunk cmem
                       in newState s { stack = ss,
                                       dict = dict',
                                       defining = Just $ Defining V.empty []
                                                             (ForthWord name False linkhead wid abort) }
                 otherwise -> abortWith "missing word name" s

-- Helper for smudge, terminate defining of a word and make it available.
smudge' :: Cell cell => FM cell ()
smudge' = updateState $ \s ->
  case defining s of
    Nothing -> abortWith "not defining" s
    Just defining  ->
      let dict' = (dict s) { latest = Just word }
          word = (definingWord defining) { doer = codeField }
          codeVector = compileList defining
          codeField = docol $ V.toList codeVector
      in newState s { defining = Nothing,
                      dict = dict' }

backslash :: Cell cell => FM cell ()
backslash = docol body
  where body = toIn : fetch : inputLine : fetch : over : plus : inputLineLength : fetch : rot : minus : loop
        loop = lit (Val 1) : minus : dup : branch0 eol : over : cfetch : lit (Val 10) : minus : branch0 found : swap : lit (Val 1) : plus : swap : branch loop : eol
        eol = drop : drop : inputLineLength : fetch : toIn : store : semi : found
        found = [inputLineLength, fetch, swap, minus, toIn, store, drop, semi]
