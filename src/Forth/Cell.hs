{-
  This file is part of Planet Pluto Forth.
  Copyright Håkan Thörngren 2011-2013

  Type class for primary unit of information.

-}

module Forth.Cell (Cell(..)) where

import Data.Bits
import Data.Int
import Util.Endian


-- | Cell class
class (Bits cell, Integral cell, Num cell) => Cell cell

instance Cell Int16
instance Cell Int32
