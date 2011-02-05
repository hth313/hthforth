{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth target configuration.

-}

module Forth.Configuration (Configuration(..), Endian(..), newConfiguration) where

import Data.Bits
import Data.Word
import Forth.Cell

-- Configuration of a target. Endianess and number of bytes per cell are needed
-- information. The internal representation of a
data Cell cell =>
    Configuration cell = Configuration { bytesPerCell :: cell,
                                         toBytes :: cell -> [Word8],
                                         toValue :: [Word8] -> cell,
                                         endian :: Endian }

data Endian = LittleEndian | BigEndian

newConfiguration :: Cell cell =>
                    cell -> Endian -> Configuration cell
newConfiguration cellSize endian = Configuration cellSize toBytes toValue endian
    where toBytes val = map (fromIntegral.(shiftR val)) shifts
          toValue bytes = sum $ map (uncurry shiftL) (zip (map fromIntegral bytes) shifts)
          shifts = case endian of
                     LittleEndian -> bits
                     BigEndian -> reverse bits
          bits = take (fromIntegral cellSize) [0,8..]
