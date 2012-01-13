{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Basic types.

-}

module Forth.Types (DataField(..), DataObject(..), ColonSlice, ForthCell(..),
                    ForthCells, ColonElement(..), Key(..), Construct(..)) where

import Forth.Cell
import Data.Map (Map)
import Data.Word
import qualified Data.Vector as V
import Data.Vector (Vector)

-- | Key type to identify a Forth word internally
newtype Key = Key Int deriving (Show, Eq, Ord)

-- | A part describes either the header or body of a word
data Part = Header | Body deriving (Eq, Ord, Show)

-- | Address offset (inside a word) is same as Int which is enough and what Data.Vector
--   wants to work with.
type AddressOffset = Int

-- | An address describes an addressable cell element of a word.
data Adr = Adr Key Part AddressOffset deriving (Eq, Ord)
instance Show Adr where
    show (Adr key part offset) = "<" ++ show key ++ ", " ++ show part ++ ", " ++ show offset ++ ">"

-- | Values that can be stored inside a cell, like on the stack.
data Cell cell => ForthCell cell = Continuation (ColonSlice cell)  |
                                   Address Adr | Val cell |
                                   ExecutionToken Key |
                                   Bottom String |
                                   UndefinedValue deriving (Eq, Show)
type (ForthCells cell) = [ForthCell cell]

-- | The contents of a colon definition body. A vector is used here to allow for easy
--   navigation back and forth when using control structures.
type ColonSlice cell = Vector (ColonElement cell)

-- An element of a colon definition
data Cell cell => ColonElement cell = WordRef Key |
                                      Literal (ForthCell cell) |
                                      Structure Construct |
                                      CondBranch Bool AddressOffset |
                                      Branch AddressOffset |
                                      Begin AddressOffset |
                                      While AddressOffset |
                                      NOP -- empty placeholder
                                      deriving (Show, Eq)
data Construct = IF | ELSE | THEN | BEGIN | WHILE | REPEAT | UNTIL deriving (Show, Eq)

-- DataObject is what is written to memory, it will either be a cell or char value.
-- Thus, it introduces a wrapper for these alternatives.
data Cell cell => DataObject cell = Cell (ForthCell cell) | Byte Word8 | Undefined

-- A data field is the body of a data word
data Cell cell => DataField cell = DataField { dataSize :: cell,
                                               writable :: Bool,
                                               objects :: Map cell (DataObject cell) }
