{-

  GNU style assembler directives.

-}

module Translator.Assembler.Directive (GNUDirective(..)) where

import Control.Applicative
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char
import Translator.Assembler.InstructionSet
import Translator.Expression
import Numeric
import Prelude hiding (showChar)


data GNUDirective = BYTE [Expr]
                  | WORD [Expr]
                  | LONG [Expr]
                  | ASCII [ByteString]
                  | FILL [Expr]
                  | SECTION String [Char]
                  | TEXT (Maybe Int)

instance InstructionSet GNUDirective where
  disassemble (BYTE vals)     = (".byte",    Just (map show vals))
  disassemble (WORD vals)     = (".word",    Just (map show vals))
  disassemble (LONG vals)     = (".long",    Just (map show vals))
  disassemble (ASCII strings) = (".ascii",   Just (map (show . concatMap showChar . C.unpack) strings))
  disassemble (FILL ns)         = (".fill",    Just (map show ns))
  disassemble (SECTION name []) = (".section", Just [name])
  disassemble (SECTION name fs) = (".section", Just [name, show fs])
  disassemble (TEXT n)          = (".text",    (\n -> [show n]) <$> n)

-- | Show character in a way that is (hopefully) compatible with C and various
--   assemblers.
showChar :: Char -> String
showChar c | c < ' ' = case c of
                         '\010' -> "\n"
                         '\014' -> "\f"
                         '\012' -> "\n"
                         '\015' -> "\r"
                         '\011' -> "\t"
                         otherwise -> octal
           | c <= '~' = [c]
           | otherwise = octal
  where octal = '\\' : showOct (ord c) ""
