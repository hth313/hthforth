{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}

module Forth.Machine (ForthLambda, Key(..), Machine(..), ColonElement(..),
                      ColonSlice, ForthWord(..), Body(..), ForthValue(..),
                      update, addWord) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy
import Forth.Configuration
import Forth.DataField

update :: (Machine cell -> Machine cell) -> ForthLambda cell
update f = StateT (\s -> return ((), f s))

addWord word = update (\s ->
    let (key : keys') = keys s
        word' = word  key
    in s { wordKeyMap = Map.insert key word' (wordKeyMap s),
           wordNameMap = Map.insert (wordName word') key (wordNameMap s),
           keys = keys' })

-- The Forth state
data Machine cell = Machine { -- The Forth stacks
                              stack, rstack :: ForthValues cell,
                              -- Word lookup maps
                              wordKeyMap :: Map Key (ForthWord cell),
                              wordNameMap :: Map String Key,
                              -- The interpretive pointer
                              ip :: ColonSlice cell,
                              conf :: Configuration cell,
                              keys :: [Key] }

-- Key type to identify a Forth word internally
newtype Key = Key Int deriving (Show, Eq, Ord)

-- An element of a colon definition
data ColonElement cell  = WordRef Key |
                          Literal (ForthValue cell) |
                          Branch (ColonSlice cell)
                          deriving (Show, Eq)

-- The contents of a colon definition body
type (ColonSlice cell)  = [ColonElement cell]

-- A Forth word, contains the header and its associated body
data ForthWord cell = ForthWord { wordName :: String,
                                  immediate :: Bool,
                                  body :: Body cell,
                                  wordKey :: Key }

-- The body of a Forth word
data Body cell = Code { -- native Haskell version
                        lambda :: Maybe (ForthLambda cell),
                        -- Native code for a given target
                        targetNative :: Maybe [Instruction],
                        -- Colon definition version
                        colon :: Maybe (ColonSlice cell) } |
                 Data (DataField cell)

-- Values that can be stored on the stack
data ForthValue cell = Continuation (ColonSlice cell)  |
                       Address Key Int | Val cell deriving (Eq, Show)
type (ForthValues cell) = [ForthValue cell]

-- A Forth native lambda should obey this signature
type (ForthLambda cell)  = StateT (Machine cell) IO ()

-- Temporary, replace with something that can actually represent a slice
-- of assembler code.
data Instruction = String