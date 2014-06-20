{-# LANGUAGE FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE MultiWayIf, PatternGuards #-}
{- |

   The Forth interpreter.

-}

module Language.Forth.Interpreter (initialState, initialVarStorage, quit) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.IntMap as IntMap
import qualified Data.Vector as V
import System.Console.Haskeline
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
import Language.Forth.WordId

initialState :: Cell cell => Target cell -> FState cell
initialState target = FState [] [] [] target newDictionary IntMap.empty

initialVarStorage :: Cell cell => FM cell ()
initialVarStorage = gets target >>=
  \t -> let f (wid, val) =
              let field@(DataField cm) = newDataField t wid (bytesPerCell t)
              in  putField wid (DataField $ writeCell val (Addr wid 0) cm)
        in mapM_ f [(sourceWId, 0), (stateWId, 0), (toInWId, 0),
                    (inputLineWId, 0), (inputLineLengthWId, 0)]

-- | Foundation of the Forth interpreter
instance Cell cell => Primitive (CV cell) (FM cell ()) where
  semi = call =<< rpop
  execute = call =<< dpop
  lit val = dpush val >> next
  swap = updateState $ \s -> case stack s of
                               s0 : s1 : ss -> newState s { stack = s1 : s0 : ss }
                               otherwise -> emptyStack s
  fetch = fetch'
  store = store'
  add = (dpush =<< (liftM2 (+) dpop dpop)) >> next
  quit = docol [ modify $ \s -> s { rstack = [], stack = Val 0 : stack s },
                 sourceId, store, mainLoop ]
  interpret = return ()
  docol (x:xs) = modify (\s -> s { ip = xs }) >> x >> next
  docol [] = semi
  branch = docol
  branch0 loc = dpop >>= \n -> if | isZero n -> docol loc
                                  | otherwise  -> next
  -- variables
  state           = pushAdr stateWId
  sourceId        = pushAdr sourceWId
  toIn            = pushAdr toInWId
  inputBuffer     = pushAdr inputBufferWId
  inputLine       = pushAdr inputLineWId
  inputLineLength = pushAdr inputLineLengthWId

-- Execute from a name
x name = searchDict name >>= \case
            Just word -> doer word
            Nothing -> abort

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
        in docol [ putField inputBufferWId (textBuffer inputBufferWId line),
                   push (Val 0), toIn, store,
                   pushAdr inputBufferWId, inputLine, store,
                   push (Val $ fromIntegral $ C.length line), inputLineLength, store,
                   interpret, liftIO $ putStrLn "ok", mainLoop]

-- | Insert the field contents of given word
putField :: Cell cell => WordId -> DataField cell (FM cell ()) -> FM cell ()
putField wid field = modify $ \s -> s { variables = IntMap.insert (unWordId wid) field  (variables s) }

-- | Push a value on data stack
push :: Cell cell => CellVal cell (FM cell ()) -> FM cell ()
push x = modify $ \s -> s { stack = x : stack s }

-- | Push the field address of a word on stack
pushAdr :: Cell cell => WordId -> FM cell ()
pushAdr wid = push $ Address (Just $ Addr wid 0)

abort :: Cell cell => FM cell ()
abort = quit

abort0 :: Cell cell => FM cell (CV cell)
abort0 = abort >> return (Val 0)

next :: Cell cell => FM cell ()
next = gets ip >>= docol

call :: Cell cell => CV cell -> FM cell ()
call (XT name) = abort
call _ = abort

-- Data stack primitives
dpush :: CV cell -> FM cell ()
dpush val = modify $ \s -> s { stack = val : stack s }

dpop :: Cell cell => FM cell (CV cell)
dpop = gets stack >>= \case
           [] -> abort0
           x:xs -> modify (\s -> s { stack = xs }) >> return x

rpop :: Cell cell => FM cell (CV cell)
rpop = gets rstack >>= \case
           [] -> abort0
           x:xs -> modify (\s -> s { rstack = xs }) >> return x

updateState f = do
  result <- StateT f
  case result of
    Left msg -> abortMessage msg
    Right x -> return x

updateStateVal x f = do
  result <- StateT f
  case result of
    Left msg -> abortMessage msg >> return x
    Right y -> return y

newState s = return (Right (), s)

emptyStack = abortWith "empty stack"
abortWith msg s = return (Left msg, s)
abortMessage msg = liftIO (putStrLn msg) >> abort

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
