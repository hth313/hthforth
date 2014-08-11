{-

  GNU style assembler directives.

-}

module Translator.Assembler.Directive (GNUDirective(..)) where

import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C
import Translator.Assembler.InstructionSet
import Translator.Expression

data GNUDirective = BYTE [Expr]
                  | WORD [Expr]
                  | LONG [Expr]
                  | ASCII [ByteString]

instance InstructionSet GNUDirective where
  disassemble (BYTE vals)     = (".byte", Just (map show vals))
  disassemble (WORD vals)     = (".word", Just (map show vals))
  disassemble (LONG vals)     = (".long", Just (map show vals))
  disassemble (ASCII strings) = (".ascii", Just (map (show . C.unpack) strings))
