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
import Forth.Configuration
import Forth.Machine
import Forth.Parser

instance Cell Int32

main =
    let core1 = do
          nativeWords
          loadScreens "/Users/hth/projects/CalcForth/src/lib/core.fth"
          load 100
    in do
      let cellSize = 4 :: Int32
      evalStateT core1 (initialState (newConfiguration cellSize LittleEndian) parseForth)
