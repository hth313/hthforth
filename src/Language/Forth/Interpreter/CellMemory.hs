{-

  Implement an addressable symbolic memory that can store cells as
  well as native bytes.

-}

module Language.Forth.Interpreter.CellMemory (CellMemory, newCellMemory,
                                              readCell, writeCell,
                                              updateDataPointer) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Word
import Language.Forth.Interpreter.Address
import Language.Forth.CellVal
import Language.Forth.Target
import Util.Address
import Util.Endian

data CellMemory cell a = CellMemory
  { contents :: IntMap (StorageUnit cell a)
  , memSize :: Int
  , target :: Target cell
  , dpOffset :: Int
  }

data StorageUnit cell a = Part Int (CellVal cell a) | Byte Word8

newCellMemory :: Target cell -> Int -> CellMemory cell a
newCellMemory target size = CellMemory IntMap.empty size target 0

readCell :: Addr -> CellMemory cell a -> Maybe (CellVal cell a)
readCell (Addr _ i) mem =
    case IntMap.lookup i (contents mem) of
      Just (Part _ cell) -> Just cell

writeCell :: CellVal cell a -> Addr -> CellMemory cell a -> CellMemory cell a
writeCell val (Addr _ i) mem =
    mem { contents = IntMap.insert i (Part 0 val) (contents mem) }

-- | Apply a function to the datapointer to advance it, return the
--   previous value of it
updateDataPointer :: (Int -> Int) -> CellMemory cell a -> (Int, CellMemory cell a)
updateDataPointer f mem = (dpOffset mem, mem { dpOffset = f (dpOffset mem) })
