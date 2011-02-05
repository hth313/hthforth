{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  The cell class.

-}

module Forth.Cell (Cell) where

import Data.Bits

-- Cell class
class  (Eq cell, Show cell, Bits cell, Ord cell, Num cell, Integral cell) => (Cell cell)

