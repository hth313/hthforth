{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}

module Forth.Machine (ForthLambda, Key(..), Machine(..), ColonElement(..),
                      ColonSlice, ForthWord(..), Body(..), ForthValue(..),
                      StateT(..), Construct(..),
                      liftIO, lift, wordFromName, initialState, evalStateT,
                      loadScreens, load,
                      update, readMachine, addWord, cellSize) where

import Data.Bits
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State.Lazy
import Forth.Configuration
import Forth.Cell
import Forth.DataField hiding (conf)
import Forth.Block
import Forth.Cell
import Text.Parsec.Error
import Numeric

update :: Cell cell => (Machine cell -> Machine cell) -> ForthLambda cell
update f = StateT (\s -> return ((), f s))

readMachine f = StateT (\s -> return (f s, s))

initialState conf parser =
    Machine [] [] Map.empty Map.empty [] conf Nothing Map.empty parser []

-- | Add a new Forth word
addWord :: Cell cell => (Key -> ForthWord cell) -> ForthLambda cell
addWord word = update (\s ->
    let (key : keys') = keys s
        word' = word key
        finalWord = word' { body = fmap (compileStructure key) (body word') }
    in s { wordKeyMap = Map.insert key finalWord (wordKeyMap s),
           wordNameMap = Map.insert (wordName finalWord) key (wordNameMap s),
           keys = keys' })

-- Compile structures like IF-ELSE-THEN into branch primitives
compileStructure key body@(Code _ _ (Just colon)) =
    let numbered = zip [1..] colon
        visit (stack, colon) (n, Structure IF) = ((n, CondBranch False) : stack, colon)
        visit ((nif, branch) : stack, colon) (nelse, Structure ELSE) =
              let colon' = IntMap.update (const (Just (branch (key, nelse + 1)))) nif colon
              in ((nelse, Branch) : stack, colon')
        visit ((n, branch) : stack, colon) (nthen, Structure THEN) =
              let colon' = IntMap.update (const (Just (branch (key, nthen + 1)))) n colon
                  colon'' = IntMap.update (const (Just NOP)) nthen colon'
              in (stack, colon'')
        (stack', colon') = foldl visit ([], IntMap.fromList numbered) numbered
        colonInt = IntMap.elems colon' -- TODO: replace with ColonSlice?
    in body { colon = Just (IntMap.elems colon') }

-- | Given the name of a word, look it up in the dictionary and return its
--   compiled inner representation
wordFromName :: Cell cell =>
                String -> StateT (Machine cell) IO (Maybe (ColonElement cell))
wordFromName name =
    StateT (\s ->
        case Map.lookup name (wordNameMap s) of
          Just wordkey -> return (Just $ WordRef wordkey, s)
          Nothing ->
              let literal n = return (Just $ Literal (Val n), s)
              in case readDec name of
                   [(n,"")] -> literal n
                   otherwise ->
                       case readSigned readDec name of
                         [(n,"")] -> literal n
                         otherwise -> return (Nothing, s))

cellSize :: Cell cell => StateT (Machine cell) IO cell
cellSize = StateT (\s -> return (bytesPerCell (conf s), s))

-- | Load an entire set of screens from a file
loadScreens :: Cell cell => FilePath -> StateT (Machine cell) IO ()
loadScreens filepath = do
    (blocks, shadows) <-
        liftIO $ readBlockFile "/Users/hth/projects/CalcForth/src/lib/core.fth"
    liftIO $ putStrLn (show blocks)
    update (\s -> s { screens = blocks })

-- | Load a screen
load :: Cell cell => Int -> StateT (Machine cell) IO (Either ParseError ())
load n = do
  (text, parser) <- readMachine (\s -> (Map.lookup n (screens s), forthParser s))
  case text of
    Just text -> parser ("screen " ++ show n) text

-- The Forth state
data Cell cell =>
    Machine cell = Machine { -- The Forth stacks
                             stack, rstack :: ForthValues cell,
                             -- Word lookup maps
                             wordKeyMap :: Map Key (ForthWord cell),
                             wordNameMap :: Map String Key,
                             -- The interpretive pointer
                             ip :: ColonSlice cell,
                             conf :: Configuration cell,
                             -- Most recent word, or word being defined
                             lastWord :: Maybe Key,
                             screens :: Map Int String,
                             forthParser :: String -> String -> StateT (Machine cell) IO (Either ParseError ()),
                             --                              inputStream :: String,
                             -- Unique identities for words
                             keys :: [Key] }

-- Key type to identify a Forth word internally
newtype Key = Key Int deriving (Show, Eq, Ord)

-- An element of a colon definition
data Cell cell => ColonElement cell = WordRef Key |
                                      Literal (ForthValue cell) |
                                      Structure Construct |
                                      CondBranch Bool Destination |
                                      Branch Destination |
                                      NOP -- empty placeholder
                                      deriving (Show, Eq)
type Destination = (Key, Int)  -- word and offset from current location

data Construct = IF | ELSE | THEN deriving (Show, Eq)

-- The contents of a colon definition body
type (ColonSlice cell)  = [ColonElement cell]

-- A Forth word, contains the header and its associated body
data Cell cell => ForthWord cell = ForthWord { wordName :: String,
                                               immediate :: Bool,
                                               body :: Maybe (Body cell),
                                               wordKey :: Key }

-- The body of a Forth word
data Cell cell => Body cell = Code { -- native Haskell version
                                     lambda :: Maybe (ForthLambda cell),
                                     -- Native code for a given target
                                     targetNative :: Maybe [Instruction],
                                     -- Colon definition version
                                     colon :: Maybe (ColonSlice cell) } |
                              Data (DataField cell)

-- Values that can be stored on the stack
data Cell cell => ForthValue cell = Continuation (ColonSlice cell)  |
                                    Address Key cell | Val cell deriving (Eq, Show)
type (ForthValues cell) = [ForthValue cell]

-- A Forth native lambda should obey this signature
type (ForthLambda cell)  = StateT (Machine cell) IO ()

-- Temporary, replace with something that can actually represent a slice
-- of assembler code.
data Instruction = String