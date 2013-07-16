{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Type class for primary unit of information.

-}

module Forth.Cell (Cell(..)) where

import Data.Bits
import Data.Word
import Util.Endian


-- | Cell class
class (Bits cell, Integral cell, Num cell) => Cell cell where
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
