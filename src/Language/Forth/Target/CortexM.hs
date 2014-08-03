{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, OverloadedStrings #-}
{-
  Forth target code generator for Cortex-M.

  Code generation is done by instantiating the Primitive type class which results
  from a cross compilation.

-}

module Language.Forth.Target.CortexM (dictionaryCortexM, codeGenerateCortexM) where

import Data.Int
import Data.Monoid hiding (Any)
import Language.Forth.Cell
import Language.Forth.CellVal
import Language.Forth.CodeGenerate
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.Word
import qualified Translator.Expression as E
import Translator.Assembler.Generate
import Translator.Assembler.Target.ARM

-- Registers assigned for specific use
ip = R9
rstk = R11
w = R1
tbl = R7

-- | Our initial dictionary.
dictionaryCortexM :: Dictionary (IM ARMInstr)
dictionaryCortexM = newDictionary extras
  where extras = return (WordId 0)

-- | CortexM instantiation of the Forth tagless final style typeclass.
instance Primitive (CellVal Int32 (IM ARMInstr)) (IM ARMInstr) where
  exit = insRec (ldr ip (PostIndexed rstk 4)) <> insRec next <> helpers
  execute = insRec next
  lit c = insRec (LONG [cellToExpr c]) <> insRec next
  swap = insRec next
  drop = insRec next
  dup = insRec next
  over = insRec next
  rto = insRec next
  tor = insRec next
  rfetch = insRec next
  fetch = insRec next
  cfetch = insRec next
  store = insRec next
  cstore = insRec next
  plus = insRec next
  minus = insRec next
  and = insRec next
  or = insRec next
  xor = insRec next
  twoStar = insRec next
  twoSlash = insRec next
  lshift = insRec next
  rshift = insRec next
  zerop = insRec next
  lt0 = insRec next
  constant = insRec next
  umstar = insRec next
  ummod = insRec next

helpers  = insLabRec "docol" (str ip (PreIndexed rstk 4)) <>
           insRec (mov ip (RegOp R0)) <>
           insLabRec "next" (ldrh w (PostIndexed ip 2)) <>
           insRec (ldr w (RegRegInd tbl w (OpLSL 2))) <>
           insRec (ldr PC (PostIndexed w 4))

cellToExpr (Val n) = E.Value $ fromIntegral n
next = b (Mem $ E.Identifier "next")

-- | Generate code for a dictionary for Cortex-M
codeGenerateCortexM :: Dictionary (IM ARMInstr) -> IM ARMInstr
codeGenerateCortexM dict = codeGenerate ASCII dict
