{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Data field definition.

-}

module Forth.DataField (DataField(..), DataObject(..), store, allot) where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Forth.Configuration

data DataField cell = DataField { dataSize :: cell,
                                  conf :: Configuration cell,
                                  objects :: Map cell (DataObject cell) }

data DataObject cell = Cell cell | Byte Word8

-- Allocate a data field of the given size
allot n conf = DataField n conf Map.empty

-- Store a given value.
-- When writing a cell, kill any bytes it overlaps.
-- When writing a byte, kill any cell that overlaps it.
store obj offset field =
    let n = fromIntegral $ bytesPerCell (conf field)
        (eraser, offsets) =
            case obj of
              Cell _ -> (Map.delete, [offset + 1..])
              Byte _ -> (Map.update f, [(1 + offset - n)..])
                        where f (Cell _) = Nothing
                              f b = Just b
        objects' = foldr eraser (objects field) (take (n - 1) offsets)
    in field { objects = Map.insert offset obj objects' }
