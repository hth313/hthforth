{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Main module.

-}

module Main (main) where

import Data.Int
import Forth.Block
import Forth.Configuration
import Forth.Machine
import Forth.Parser

main = do
    (blocks, shadows) <- readBlockFile "/Users/hth/projects/CalcForth/src/lib/core.fth"
    let cellSize = 4 :: Int32
    evalStateT (parseForth "SCR FOO" ": FOO ;" ) (initialState (newConfiguration cellSize LittleEndian))
