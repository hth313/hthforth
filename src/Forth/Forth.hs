{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Main module.

-}

module Main (main) where

import Data.Int
import Forth.Cell
import Forth.Core
import Forth.Machine
import Forth.Target
import Util.Endian

main =
    let target = Target 4 4 1 LittleEndian :: Target Int32
    in evalStateT (addNatives >> quit) (initialState target)
