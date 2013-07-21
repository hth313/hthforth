{-

  Implement an addressable symbolic memory that can store cells as
  well as native bytes.

-}

module Forth.CellMemory (CellMemory, newCellMemory, readCell, writeCell) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word
import Forth.Address
import Forth.Target
import Forth.Types
import Util.Address
import Util.Endian

data CellMemory cell = CellMemory {
      contents :: IntMap (StorageUnit cell),
      memSize :: Int,
      target :: Target cell
    }

data StorageUnit cell = Part Int (Lit cell) | Byte Word8

newCellMemory :: Target cell -> Int -> CellMemory cell
newCellMemory target size = CellMemory IntMap.empty size target

readCell :: Addr -> CellMemory cell -> Maybe (Lit cell)
readCell (Addr _ i) mem =
    case IntMap.lookup i (contents mem) of
      Just (Part _ cell) -> Just cell

writeCell :: Lit cell -> Addr -> CellMemory cell -> CellMemory cell
writeCell val (Addr _ i) mem =
    mem { contents = IntMap.insert i (Part 0 val) (contents mem) }