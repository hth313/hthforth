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
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V
import System.Console.Haskeline
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
initialState target = FState [] [] [] target newDictionary IntMap.empty Nothing

initialVarStorage :: Cell cell => FM cell ()
initialVarStorage = gets target >>=
  \t -> let f (wid, val) =
              let field@(DataField cm) = newDataField t wid (bytesPerCell t)
              in  putField wid (DataField $ writeCell val (Addr wid 0) cm)
        in mapM_ f [(sourceWId, 0), (stateWId, 0), (toInWId, 0),
                    (inputLineWId, 0), (inputLineLengthWId, 0)]

-- | Foundation of the Forth interpreter
instance Cell cell => Primitive (CV cell) (FM cell ()) where
  semi = rpop >>= \case
           IP ip' -> do
             modify $ \s -> s { ip = ip' }
             next
           otherwise -> abortMessage "IP not on rstack"
  execute = call =<< dpop
  lit val = dpush val >> next
  swap = updateState $ \s -> case stack s of
                               s0 : s1 : ss -> newState s { stack = s1 : s0 : ss }
                               otherwise -> emptyStack s
  drop = updateState $ \s -> case stack s of
                               s0 : ss -> newState s { stack = ss }
                               otherwise -> emptyStack s
  dup = updateState $ \s -> case stack s of
                              ss@(s0 : _) -> newState s { stack = s0 : ss }
                              otherwise -> emptyStack s
  over = updateState $ \s -> case stack s of
                               ss@(s0 : s1 : _) -> newState s { stack = s1 : ss }
                               otherwise -> emptyStack s
  rot = updateState $ \s -> case stack s of
                              s0 : s1 : s2 : ss -> newState s { stack = s2 : s0 : s1 : ss }
                              otherwise -> emptyStack s
  cfetch = cfetch'
  fetch = fetch'
  store = store'
  plusStore = docol [dup, fetch, rot, plus, swap, store, semi]
  plus = binary (+)
  minus = binary (-)
  zerop = updateState $ \s -> case stack s of
                                (Val 0) : ss         -> newState s { stack = true  : ss }
                                Val{} : ss           -> newState s { stack = false : ss }
                                Address Nothing : ss -> newState s { stack = true : ss }
                                Address{} : ss       -> newState s { stack = false : ss }
                                otherwise            -> emptyStack s
  quit = ipdo [ (modify (\s -> s { rstack = [], stack = Val 0 : stack s }) >> next),
                sourceId, store, mainLoop ]
  interpret = docol [state, fetch, dpop >>= interpret1, semi]
  docol xs = modify (\s -> s { rstack = IP (ip s) : rstack s, ip = xs }) >> next
  branch = ipdo
  branch0 loc = dpop >>= \n -> if | isZero n -> ipdo loc
                                  | otherwise  -> next
  -- variables
  state           = pushAdr stateWId
  sourceId        = pushAdr sourceWId
  toIn            = pushAdr toInWId
  inputBuffer     = pushAdr inputBufferWId
  inputLine       = pushAdr inputLineWId
  inputLineLength = pushAdr inputLineLengthWId

  -- Compiling words
  create = docol [xword, create', semi]
  colon = docol [push (Val (-1)), state, store, create, semi]
  semicolon = docol [compile (XT semi), push (Val 0), state, store, smudge, semi]
  compileComma = dpop >>= compile
  smudge = smudge'

searchDict :: Cell cell => ByteString -> FM cell (Maybe (ForthWord (FM cell ())))
binary op = updateState $ \s -> case stack s of
                                  op1 : op2 : ss -> newState s { stack = op1 `op` op2 : ss }
                                  otherwise -> emptyStack s

searchDict n = gets (f . latest . dict)
  where f jw@(Just word) | n == name word = jw
                         | otherwise = f (link word)
        f Nothing = Nothing

ipdo :: Cell cell => [FM cell ()] -> FM cell ()
ipdo ip' = modify (\s -> s { ip = ip' }) >> next

mainLoop :: Cell cell => FM cell ()
mainLoop = do
  mline <- lift $ getInputLine ""
  case mline of
    Nothing -> return ()
    Just input ->
        let line = C.pack input
        in ipdo [ putField inputBufferWId (textBuffer inputBufferWId line) >> next,
                  push (Val 0), toIn, store,
                  pushAdr inputBufferWId, inputLine, store,
                  push (Val $ fromIntegral $ C.length line), inputLineLength, store,
                  interpret, liftIO (putStrLn "ok") >> next, mainLoop]

interpret1 :: Cell cell => CV cell -> FM cell ()
interpret1 stateFlag =
  let compiling = stateFlag /= Val 0
      parseNumber = parse =<< countedText =<< dpop where
        parse bs = case readDec text of
                     [(x,"")]
                       | compiling -> compile $ lit (Val x)
                       | otherwise -> push $ Val x
                     otherwise -> abortMessage $ text ++ " ?"
                     where text = C.unpack bs
      interpret2 True  = docol [drop, semi]
      interpret2 False = docol [find, dpop >>= interpret3, semi]
      interpret3 (Val 0) = docol [parseNumber, interpret, semi]
      interpret3 (Val 1) = docol [execute, interpret, semi]
      interpret3 _ | compiling = docol [xtpop >>= compile, interpret, semi] -- normal word found
                   | otherwise = docol [execute, interpret, semi]
      xtpop = dpop >>= \case
                XT word -> return (doer word)
                otherwise -> abortMessage "expected execution token" >> return next
  in docol [xword, dup, cfetch, liftM (Val 0 ==) dpop >>= interpret2, semi]

-- | Insert the field contents of given word
putField :: Cell cell => WordId -> DataField cell (FM cell ()) -> FM cell ()
putField wid field = modify $ \s -> s { variables = IntMap.insert (unWordId wid) field  (variables s) }

-- | Push a value on data stack
push :: Cell cell => CV cell -> FM cell ()
push x = do
  modify $ \s -> s { stack = x : stack s }
  next

-- | Push the field address of a word on stack
pushAdr :: Cell cell => WordId -> FM cell ()
pushAdr wid = push $ Address (Just $ Addr wid 0)

abort :: Cell cell => FM cell ()
abort = docol [modify (\s -> s { stack = [], defining = Nothing }) >> next,
               push (Val 0), state, store, quit]

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
xword :: Cell cell => FM cell ()
xword = docol [inputLine, fetch, toIn, fetch, plus, parseName, toIn, plusStore, semi]
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
