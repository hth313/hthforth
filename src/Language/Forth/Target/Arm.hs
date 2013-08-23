{-
  This file is part of Planet Pluto Forth.
  Copyright Håkan Thörngren 2013

  Configure to be used on an Arm device.

-}

module Language.Forth.Target.Arm () where

import Data.Int
import Language.Forth.Cell

instance Cell Int32 where
    bytesPerCell _        = 4
    bytesPerInstruction _ = 4
    bytesPerChar _        = 1
    endianess _           = LittleEndian