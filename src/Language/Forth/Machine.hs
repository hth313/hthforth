{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, PatternGuards #-}
{-|

  Forth virtual machine and primitives.

-}

module Language.Forth.Machine (MachineM, ForthLambda, Machine(..), push, pop, pushAdr,
                               ForthWord(..), StateT(..), emptyStack, abortWith, abortMessage,
                               updateState, updateStateVal, newState,
                               initialState, evalStateT, execute,
                               create, makeImmediate, smudge,
                               addNative, addNativeFixed, addFixed, addVar, putField,
                               wordBufferId,
                               inputBufferId, inputBufferPtrId, inputBufferLengthId,
                               stateId, sourceId, toInId, exitId,
                               wordIdExecute, wordLookup,
                               doColon, doVar, doConst,
                               withTempBuffer, addrString, searchDictionary,
                               compileWord, compile, comma) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Vector as V
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Data.Word
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Language.Forth.Address
import Language.Forth.Cell
import Language.Forth.CellMemory
import Language.Forth.CellVal
import {-# SOURCE #-} Language.Forth.Core
import Language.Forth.DataField
import Language.Forth.Target
import Language.Forth.Word
import Language.Forth.WordId
import System.Console.Haskeline

type MachineM cell = StateT (Machine cell) (InputT IO)

-- The Forth state
data Machine cell = Machine { -- The Forth stacks
                              stack, rstack :: [CellVal cell],
                              dictionaryHead :: LinkField cell,
                              defining :: LinkField cell,  -- ^ word being defined
                              ip :: Maybe (IP cell),
                              target :: Target cell,
                              -- Sequence of identies to allocate from
                              keys :: [WordId],
                              -- Data fields for words that need it. This need
                              -- to be modifiable as we mimic data ram. We
                              -- rely on that the underlaying identity of
                              -- a WordId is an Int here.
                              variables :: IntMap (DataField cell),
                              wordMap :: IntMap (ForthWord cell),
                              oldHandles :: [WordId],
                              stringLiterals :: Map ByteString Addr
    }

-- A Forth native lambda should obey this signature
type ForthLambda cell = MachineM cell ()

-- | Internal words that does not need to be redefinable can share
--   this identity.
pseudoId = 0

-- | WordId used for special purposes
wordBufferId = 1 :: Int     -- ^ Transient area for WORD
inputBufferId = 2 :: Int    -- ^ Input buffer (console)
inputBufferPtrId = 3 :: Int  -- ^ Variable that point out current input buffer
inputBufferLengthId = 4 :: Int    -- ^ Input buffer length
stateId = 5 :: Int          -- ^ Compile state
sourceId = 6 :: Int         -- ^ SOURCE-ID
toInId = 7 :: Int           -- ^ >IN
exitId = 8 :: Int

-- The first dynamic word identity
firstId = 9

-- | Lookup a word from its identity number
wordIdExecute wid = do
  w <- wordLookup wid
  case w of
    Just w -> call w

wordLookup :: WordId -> MachineM cell (Maybe (ForthWord cell))
wordLookup wid = gets $ \s -> IntMap.lookup wid (wordMap s)

call word = doer word word

-- | Top level item on stack should be an execution token that is invoked.
execute :: Cell cell => ForthLambda cell
execute = pop >>= executeXT

executeXT (XT word) = call word
executeXT x = abortMessage $ "EXECUTE expects an execution token, got " ++ show x

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

-- | Pop from data stack
pop :: Cell cell => MachineM cell (CellVal cell)
pop = updateStateVal (Val 0) $ \s ->
        case stack s of
          t:ts -> return (Right t, s { stack = ts })
          [] -> emptyStack s

emptyStack = abortWith "empty stack"

abortWith msg s = return (Left msg, s)

abortMessage msg = liftIO (putStrLn msg) >> abort
-- | Push a value on data stack
push x = modify $ \s -> s { stack = x : stack s }


-- | Push the field address of a word on stack
pushAdr wid = push $ Address (Just (Addr wid 0))


-- | Create an initial empty Forth machine state
initialState :: Target cell -> Machine cell
initialState target =
    Machine [] [] Nothing Nothing Nothing target [firstId..]
            IntMap.empty IntMap.empty [] Map.empty


create :: ByteString -> (ForthWord cell -> ForthLambda cell) -> MachineM cell ()
create name does = modify $ \s ->
    let k:ks = keys s
        word = ForthWord name False (dictionaryHead s) k does (Colon V.empty)
    in s { keys = ks,
           defining = Just word }


-- | Make word being defined visible in the dictionary
smudge :: ForthLambda cell
smudge = modify $ \s ->
    case defining s of
      Just word -> s { dictionaryHead = Just word,
                       defining = Nothing }
      otherwise -> s


makeImmediate :: ForthLambda cell
makeImmediate = modify $ \s -> s { dictionaryHead = imm <$> dictionaryHead s }
    where imm word = word { immediate = True }


-- | Add a native word to the vocabulary.
addNative :: ByteString -> ForthLambda cell -> MachineM cell ()
addNative name action = create name (const action) >> smudge


addNativeFixed :: WordId -> ByteString -> ForthLambda cell -> MachineM cell ()
addNativeFixed fixedId name action = do
  addNative name action
  modify $ \s ->
      let Just word = dictionaryHead s
          newWord = word { wid = fixedId }
      in s { keys = wid word : keys s,
             dictionaryHead = Just newWord,
             wordMap = IntMap.insert fixedId newWord $ IntMap.delete (wid word) (wordMap s) }


addVar name wid mval = do
  addFixed name False wid doVar
  smudge
  case mval of
    Nothing -> return ()
    Just val -> do
        t <- gets target
        let field@(DataField cm) = newDataField t wid (bytesPerCell t)
        putField wid (DataField $ writeCell val (Addr wid 0) cm)


-- | Insert the field contents of given word
putField wid field = modify $ \s -> s { variables = IntMap.insert wid field  (variables s) }


-- | Add a word with a fixed identity.
addFixed name imm wid does = modify $ \s ->
    let word = ForthWord name imm (dictionaryHead s) wid does Native
    in s { dictionaryHead = Just word,
           wordMap = IntMap.insert wid word (wordMap s) }


-- | Push the address of a variable (its data field) on stack
doVar word = push $ Address (Just (Addr (wid word) 0))

doConst word =
    case body word of
      Colon cb | not (V.null cb) ->
          push $ V.head cb
      otherwise -> abortMessage $ "constant value missing for " ++ C.unpack (name word)

doColon word = do
  oip <- StateT $ \s ->
            let Colon cb = body word
            in return (ip s, s { rstack = Loc (ip s) : rstack s, ip = Just (IP cb 0) })
  when (isNothing oip) nextInterpreter


nextInterpreter :: Cell cell => ForthLambda cell
nextInterpreter = do
  x <- next
  case x of
    Nothing -> return ()
    Just xt -> executeXT xt >> nextInterpreter


-- | Read cell pointed out by interpretive pointer, advance pointer
next = StateT $ \s ->
    case ip s of
      Just (IP cb i) -> return (Just ((V.!) cb i), s { ip = Just (IP cb (i + 1)) })
      Nothing -> return (Nothing, s { ip = Nothing })


-- | Create a temporary word with given buffer contents. Perform action by
--   passing a reference to the buffer to it.
withTempBuffer action contents = do
  handle <- getHandle
  modify $ \s ->
      s { variables = IntMap.insert handle (textBuffer handle contents) (variables s) }
  pushAdr handle
  push $ Val (fromIntegral $ C.length contents)
  action
  modify $ \s -> s { variables = IntMap.delete handle (variables s),
                                 oldHandles = handle : oldHandles s }
      where
        getHandle = StateT $ \s ->
             if null (oldHandles s)
             then return (head (keys s), s { keys = tail (keys s) })
             else return (head (oldHandles s), s { oldHandles = tail (oldHandles s) })


-- | Make a string literal addressable on the fly.
addrString text s =
    case Map.lookup text (stringLiterals s) of
      Just addr -> (Address (Just addr), s)
      Nothing ->
          let (k:ks) = keys s
              addr = Addr k 0
              in (Address (Just addr), s { keys = ks,
                                           stringLiterals = Map.insert text addr
                                                            (stringLiterals s) })


-- | Compile a literal into a colon body of the word being defined.
compile lit = updateState $ \s ->
    case defining s of
      Just word | Colon cb <- body word ->
          newState s { defining = Just word { body = Colon (V.snoc cb lit)  } }
      otherwise -> abortWith "unable to compile literal value" s


searchDictionary findname = gets $ \s ->
    let locate (Just word) | name word == findname = Just word
                           | otherwise = locate $ link word
        locate Nothing = Nothing
    in locate $ dictionaryHead s


-- | Compile a named word
compileWord name = do
  mword <- searchDictionary name
  case mword of
    Just word -> compile $ XT word
    otherwise -> abortMessage $ C.unpack name ++ " ?"

-- | Add a literal to current body of word being defined.
comma :: Cell cell => ForthLambda cell
comma = pop >>= compile
