{-# LANGUAGE PackageImports,TypeSynonymInstances #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth target configuration.

-}

module Forth.Configuration (ConfigurationM, Configuration(..), Endian(..),
                            ask, withConfiguration, newConfiguration) where

import Data.Bits
import Data.Word
import Forth.Cell
import Control.Monad.Trans.Reader
import Control.Monad.Trans

type ConfigurationM cell = ReaderT (Configuration cell) IO

withConfiguration :: Cell cell => Configuration cell ->
                     ReaderT (Configuration cell) IO a -> IO a
withConfiguration conf action = runReaderT action conf

-- Configuration of a target. Endianess and number of bytes per cell are needed
-- information. The internal representation of a
data Cell cell =>
    Configuration cell = Configuration { bytesPerCell :: cell,
                                         bytesPerInstruction :: cell,
                                         bytesPerChar :: cell,
                                         toBytes :: cell -> [Word8],
                                         toValue :: [Word8] -> cell,
                                         endian :: Endian }

data Endian = LittleEndian | BigEndian

newConfiguration :: Cell cell =>
                    cell -> cell -> cell -> Endian -> Configuration cell
newConfiguration cellSize xtSize charSize endian =
    Configuration cellSize xtSize charSize toBytes toValue endian
    where toBytes val = map (fromIntegral.(shiftR val)) shifts
          toValue bytes = sum $ map (uncurry shiftL) (zip (map fromIntegral bytes) shifts)
          shifts = case endian of
                     LittleEndian -> bits
                     BigEndian -> reverse bits
          bits = take (fromIntegral cellSize) [0,8..]
