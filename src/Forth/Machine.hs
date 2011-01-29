{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}


module Forth.Machine (ForthLambda, Key(..), Machine(..), ColonElement(..),
                      ColonSlice, ForthWord(..), Body(..), ForthValue(..),
                      update, addWord, cellBytes) where

import Data.Bits
import Data.Word
import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

update :: (Machine -> Machine) -> ForthLambda
update f = StateT (\s -> return ((), f s))

addWord word = update (\s ->
    let (key : keys') = keys s
        word' = word  key
    in s { wordKeyMap = Map.insert key word' (wordKeyMap s),
           wordNameMap = Map.insert (wordName word') key (wordNameMap s),
           keys = keys' })

-- TODO: Fix endianess and Int32 as define?
cellBytes :: ForthValue -> Machine -> [Word8]
cellBytes (Val val) s = map (fromIntegral.(shiftR val)) [ 0, 8, 16, 24 ]


-- The Forth state
data Machine = Machine { -- The Forth stacks
                         stack, rstack :: ForthValues,
                         -- Word lookup maps
                         wordKeyMap :: Map Key ForthWord,
                         wordNameMap :: Map String Key,
                         -- The interpretive pointer
                         ip :: ColonSlice,
                         bytesPerCell :: Int,
                         keys :: [Key] }

-- Key type to identify a Forth word internally
newtype Key = Key Int deriving (Show, Eq, Ord)

-- An element of a colon definition
data ColonElement = WordRef Key | Literal ForthValue | Branch ColonSlice deriving (Show, Eq)

-- The contents of a colon definition body
type ColonSlice = [ColonElement]

-- A Forth word, contains the header and its associated body
data ForthWord = ForthWord { wordName :: String,
                             immediate :: Bool,
                             body :: Body,
                             wordKey :: Key }

-- The body of a Forth word
data Body = Code { -- native Haskell version
                   lambda :: Maybe ForthLambda,
                   -- Native code for a given target
                   targetNative :: Maybe [Instruction],
                   -- Colon definition version
                   colon :: Maybe ColonSlice } |
            Data [Word8]

-- Values that can be stored on the stack
data ForthValue = Continuation ColonSlice | Address Key Int | Val Int32 deriving (Eq, Show)
type ForthValues = [ForthValue]

-- A Forth native lambda should obey this signature
type ForthLambda = StateT Machine IO ()

-- Temporary, replace with something that can actually represent a slice
-- of assembler code.
data Instruction = String