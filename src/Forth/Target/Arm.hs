{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2013

  Configure to be used on an Arm device.

-}

module Forth.Target.Arm () where

import Data.Int
import Forth.Cell

instance Cell Int32 where
    bytesPerCell _        = 4
    bytesPerInstruction _ = 4
    bytesPerChar _        = 1
    endianess _           = LittleEndian
