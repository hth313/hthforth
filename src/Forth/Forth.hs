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
    let load1 = do
          nativeWords
          loadScreens "/Users/hth/projects/CalcForth/src/lib/core.fth"
          load 1
        cellSize = 4 :: Int32
        executionTokenSize = 2 :: Int32
        charSize = 1 :: Int32
        conf = newConfiguration cellSize executionTokenSize charSize LittleEndian
    in withConfiguration conf (evalStateT load1 (initialState parseForth))
