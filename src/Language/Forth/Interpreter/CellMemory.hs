{-

  Implement an addressable symbolic memory that can store cells as
  well as native bytes.

-}

module Language.Forth.Interpreter.CellMemory (CellMemory, dpOffset, StorageUnit(..),
                                              newCellMemory,
                                              readCell, writeCell, read8CM, write8CM,
                                              blockMoveTextCM, alignDP,
                                              updateDataPointer, validAddressCM) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Vector.Storable.ByteString as B
import Data.Word
import Language.Forth.Interpreter.Address
import Language.Forth.CellVal
import Language.Forth.Target
import Util.Address
import Util.Endian

data CellMemory a = CellMemory
  { contents :: IntMap (StorageUnit a)
  , target :: Target
  , dpOffset :: Int
  }

data StorageUnit a = Part Int (CellVal a) | Byte Word8

newCellMemory :: Target -> Int -> CellMemory a
newCellMemory target size = CellMemory IntMap.empty target size

readCell :: Addr -> CellMemory a -> Maybe (CellVal a)
readCell (Addr _ i) mem =
    case IntMap.lookup i (contents mem) of
      Just (Part _ cell) -> Just cell
      otherwise -> Nothing

writeCell :: CellVal a -> Addr -> CellMemory a -> CellMemory a
writeCell val (Addr _ i) mem =
    mem { contents = IntMap.insert i (Part 0 val) (contents mem) }

read8CM :: Addr -> CellMemory a -> Maybe (StorageUnit a)
read8CM (Addr _ i) mem = IntMap.lookup i (contents mem)

write8CM :: Word8 -> Addr -> CellMemory a -> CellMemory a
write8CM val (Addr _ i) mem = mem { contents = IntMap.insert i (Byte val) (contents mem) }

-- | Apply a function to the datapointer to advance it, return the
--   previous value of it
updateDataPointer :: (Int -> Int) -> CellMemory a -> (Int, CellMemory a)
updateDataPointer f mem = (dpOffset mem, mem { dpOffset = f (dpOffset mem) })

blockMoveTextCM text (Addr _ offset) cm =
  let im' = IntMap.fromList $ zip [offset..] (map Byte $ B.unpack text)
  in cm { contents = IntMap.union im' (contents cm) }

validAddressCM (Addr _ offset) cm = offset < dpOffset cm && offset >= 0

alignDP mem target = mem { dpOffset = alignOffset (dpOffset mem) target }
