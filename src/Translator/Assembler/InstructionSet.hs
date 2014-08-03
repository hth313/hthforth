{-
  Define a type class for instruction sets.
-}

module Translator.Assembler.InstructionSet where

class InstructionSet is where
  disassemble :: is -> (String, Maybe [String])
