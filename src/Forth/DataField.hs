{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Data field definition.

-}

module Forth.DataField (DataField(..), storeData, fetchData, allot) where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Forth.Cell
import Forth.Configuration
import Forth.Types

-- | Allocate a data field of the given size
allot :: Cell cell => cell -> DataField cell
allot n = DataField n True Map.empty

-- | Store a given value.
--   When writing a cell, kill any bytes it overlaps.
--   When writing a byte, kill any cell that overlaps it.
storeData :: Cell cell => DataObject cell -> cell -> DataField cell -> Configuration cell -> DataField cell
storeData Undefined _ field conf = field { objects = Map.empty }  -- remove all
storeData obj offset field conf =
    let n = bytesPerCell conf
        limitedOffsets = take (fromIntegral (n - 1)) offsets
        (eraser, offsets) =
            case obj of
              Cell _ -> (Map.delete, [offset + 1..])
              Byte _ -> (Map.update f, [(1 + offset - n)..])
                        where f (Cell _) = Nothing
                              f b = Just b
        objects' = foldr eraser (objects field) limitedOffsets
    in field { objects = Map.insert offset obj objects' }

-- | Fetch a data object from the given offset
fetchData :: Cell cell => cell -> DataField cell -> DataObject cell
fetchData offset field =
    case Map.lookup offset (objects field) of
      Nothing -> Undefined
      Just val -> val