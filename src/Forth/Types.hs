{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Basic types.

-}

module Forth.Types (DataField(..), DataObject(..), Lit(..), WordKey(..),
                    Adr(..), Dictionary) where

import Data.Map (Map)
import Data.Word
import qualified Data.Vector as V
import Data.ByteString.Char8 (ByteString)

-- | Key type to identify a Forth word internally
newtype WordKey = WordKey Int deriving (Show, Eq, Ord)

-- | Literals that can be stored inside a cell, like on the stack.
data Lit cell = Address (Adr cell) |
                Val cell |
                ExecutionToken WordKey |
                InputSource ByteString |
                Bot String |
                UndefinedValue
                deriving (Eq, Show)

-- | An address describes an addressable cell element of a word.
data Adr cell = BodyAdr WordKey cell |
                BufferAdr ByteString cell |
                DeadAdr
                deriving (Eq, Ord)

instance Show cell => Show (Adr cell)  where
    show (BodyAdr key offset) = showAddress "body" key offset
    show (BufferAdr key offset) = showAddress "buf" key offset

showAddress kind key offset = "<" ++ show key ++ "::" ++ kind ++ " " ++ show offset ++ ">"

-- DataObject is what is written to memory, it will either be a cell or char value.
-- Thus, it introduces a wrapper for these alternatives.
data DataObject cell = Cell (Lit cell) | Byte Word8 | Undefined

-- A data field is the body of a data word
data DataField cell = DataField { dataSize :: cell,
                                  writable :: Bool,
                                  objects :: Map cell (DataObject cell) }

-- | A dictionary is a list of words, most recently defined first.
type Dictionary = [WordKey]