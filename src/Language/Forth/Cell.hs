-- | Type class for primary unit of information.

module Language.Forth.Cell (Cell(..)) where

import Data.Bits
import Data.Int
import Util.Endian


-- | Cell class
class (Bits cell, Integral cell, Num cell) => Cell cell

instance Cell Int16
instance Cell Int32
