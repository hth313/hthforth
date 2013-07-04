{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Main module.

-}

module Main (main) where

import Data.Int
import Forth.Block
import Forth.Cell
import Forth.Core
import Forth.Machine


instance Cell Int32 where
    bytesPerCell        _ = 4
    bytesPerInstruction _ = 4
    bytesPerChar        _ = 1
    endianess           _ = LittleEndian

main =
    let cellSize = 4 :: Int32
        executionTokenSize = 2 :: Int32
        charSize = 1 :: Int32
    in evalStateT (nativeWords 0 >> abort) (initialState cellSize)
