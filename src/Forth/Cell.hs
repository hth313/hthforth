{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  A Forth machine word value is an instance of the Cell class.

-}

module Forth.Cell (Cell(..), Endian(..)) where

import Data.Bits
import Data.Word

-- Cell class
--class  (Eq cell, Show cell, Bits cell, Ord cell, Num cell, Integral cell) => (Cell cell)
class (Bits cell, Integral cell) => Cell cell where
    bytesPerCell :: cell -> cell           -- ^ native value size
    bytesPerInstruction :: cell -> cell    -- ^ execution token size
    bytesPerChar :: cell -> cell           -- ^ character size
    endianess :: cell -> Endian

    toBytes :: cell -> [Word8]
    toBytes val =  map (\x -> fromIntegral $ shiftR val x) (shifts val)

    toValue :: cell -> [Word8] -> cell
    toValue x bytes = sum $ map (uncurry shiftL) (zip (map fromIntegral bytes) (shifts x))


shifts x = case endianess x of
             LittleEndian -> bits
             BigEndian -> reverse bits
    where bits = take (fromIntegral (bytesPerCell x)) [0,8..]

data Endian = LittleEndian | BigEndian
