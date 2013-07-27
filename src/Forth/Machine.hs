{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}

module Forth.Machine (MachineM, ForthLambda, Machine(..), push, pop,
                      ForthWord(..), StateT(..), emptyStack, abortWith,
                      initialState, evalStateT, execute,
                      addNative, addFixed, addVar, putField,
                      wordBufferId, inputBufferId, stateId, sourceId, toInId,
                      wordIdExecute, wordLookup,
                      doColon, doVar) where

import Control.Exception
import Control.Monad.State.Lazy
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Typeable
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Forth.Address
import Forth.CellMemory
import Forth.DataField
import Forth.Target
import Forth.Word
import Forth.WordId
import Forth.Types
import Numeric
import System.IO
import Control.Exception

type MachineM cell = StateT (Machine cell) IO

-- The Forth state
data Machine cell = Machine { -- The Forth stacks
                              stack, rstack :: [Lit cell],
                              dictionaryHead :: LinkField cell,
                              ip :: IP cell,
                              target :: Target cell,
                              -- Sequence of identies to allocate from
                              keys :: [WordId],
                              -- Data fields for words that need it. This need
                              -- to be modifiable as we mimic data ram. We
                              -- rely on that the underlaying identity of
                              -- a WordId is an Int here.
                              variables :: IntMap (DataField cell),
                              wordMap :: IntMap (ForthWord cell)
    }

-- A Forth native lambda should obey this signature
type ForthLambda cell = MachineM cell ()

data ForthException = ForthException String deriving Typeable

instance Exception ForthException
instance Show ForthException where
    show (ForthException text) = text

-- | Internal words that does not need to be redefinable can share
--   this identity.
pseudoId = 0

-- | WordId used for special purposes
wordBufferId = 1 :: Int     -- ^ Transient area for WORD
inputBufferId = 2 :: Int    -- ^ Input buffer
stateId = 3 :: Int          -- ^ Compile state
sourceId = 4 :: Int         -- ^ SOURCE-ID
toInId = 5 :: Int           -- ^ >IN

-- The first dynamic word identity
firstId = 6

-- | Lookup a word from its identity number
wordIdExecute wid = do
  w <- wordLookup wid
  case w of
    Just w -> call w

wordLookup :: WordId -> MachineM cell (Maybe (ForthWord cell))
wordLookup wid = gets $ \s -> IntMap.lookup wid (wordMap s)

call word = doer word word

-- | Top level item on stack should be an execution token that is invoked.
execute = do
  word <- pop
  case word of
    XT word -> call word
    otherwise -> abortWith "EXECUTE expects an execution token"

-- | Pop from data stack
pop :: MachineM cell (Lit cell)
pop = StateT $ \s ->
        case stack s of
          t:ts -> return (t, s { stack = ts })
          [] -> emptyStack

emptyStack = abortWith "empty stack"
abortWith = throw . ForthException

-- | Push a value on data stack
push x = modify $ \s -> s { stack = x : stack s }

{-
lookupWord key = gets $ \s -> Map.lookup key (wordKeyMap s)

lookupWordFromName name = do
  Just [WordRef key] <- wordFromName name
  lookupWord key
-}

-- | Create an initial empty Forth machine state
initialState :: Target cell -> Machine cell
initialState target =
    Machine [] [] Nothing emptyIP target [firstId..] IntMap.empty IntMap.empty

-- | Add a native word to the vocabulary.
addNative :: ByteString -> ForthLambda cell -> MachineM cell ()
addNative name action = modify $ \s ->
    let k:ks = keys s
        word = ForthWord name False (dictionaryHead s) k (const action) Native
    in s { keys = ks,
           dictionaryHead = Just word }

addVar name wid mval = do
  addFixed name False wid doVar
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


doColon word = modify $ \s ->
    let Colon cb = body word
    in s { rstack = Loc (ip s) : rstack s, ip = IP cb 0 }

-- | Push the address of a variable (its data field) on stack
doVar word = push $ Address (Just (Addr (wid word) 0))
