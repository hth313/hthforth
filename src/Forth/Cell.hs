{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Type class for primary unit of information.

-}

module Forth.Cell (Cell(..)) where

import Data.Bits
import Data.Word
import Util.Endian


-- | Cell class
class (Bits cell, Integral cell, Num cell) => Cell cell
