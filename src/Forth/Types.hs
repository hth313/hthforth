{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Basic types.

-}

module Forth.Types (DataField(..), DataObject(..), ColonSlice, ForthValue(..),
                    ForthValues, ColonElement(..), Key(..), Construct(..)) where

import Forth.Cell
import Data.Map (Map)
import Data.Word

-- Key type to identify a Forth word internally
newtype Key = Key Int deriving (Show, Eq, Ord)

-- Values that can be stored on the stack
data Cell cell => ForthValue cell = Continuation (ColonSlice cell)  |
                                    Address Key cell | Val cell |
                                    Bottom String |
                                    UndefinedValue deriving (Eq, Show)
type (ForthValues cell) = [ForthValue cell]

-- The contents of a colon definition body
type ColonSlice cell = [ColonElement cell]

-- An element of a colon definition
data Cell cell => ColonElement cell = WordRef Key |
                                      Literal (ForthValue cell) |
                                      Structure Construct |
                                      CondBranch Bool Destination |
                                      Branch Destination |
                                      Begin Destination |
                                      While Destination |
                                      NOP -- empty placeholder
                                      deriving (Show, Eq)
type Destination = (Key, Int)  -- word and offset from current location

data Construct = IF | ELSE | THEN | BEGIN | WHILE | REPEAT | UNTIL deriving (Show, Eq)

-- DataObject is what is written to memory, it will either be a cell or char value.
-- Thus, it introduces a wrapper for these.
data Cell cell => DataObject cell = Cell (ForthValue cell) | Byte Word8 | Undefined

-- A data field is the body of a data word
data Cell cell => DataField cell = DataField { dataSize :: cell,
                                               writeable :: Bool,
                                               objects :: Map cell (DataObject cell) }

