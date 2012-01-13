{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  The Cell class decides on constraints on a cell type.

-}

module Forth.Cell (Cell) where

import Data.Bits

-- Cell class
class  (Eq cell, Show cell, Bits cell, Ord cell, Num cell, Integral cell) => (Cell cell)

