{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, OverloadedStrings #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-
  Forth target code generator for Cortex-M.

  Code generation is done by instantiating the Primitive type class which results
  from a cross compilation.

-}

module Language.Forth.Target.CortexM (bindCortexM, codeGenerateCortexM) where

import Control.Lens
import Data.Int
import Data.Monoid hiding (Any)
import Data.ByteString.Lazy (ByteString)
import Language.Forth.CellVal
import Language.Forth.CrossCompiler.CodeGenerate
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.TargetPrimitive
import Language.Forth.Word
import qualified Translator.Expression as E
import Translator.Assembler.Generate
import Translator.Assembler.Target.ARM

-- | Bind a polymorphic target dictionary to be an CortexM specific one
bindCortexM :: Dictionary (IM ARMInstr) -> Dictionary (IM ARMInstr)
bindCortexM = id

-- Registers assigned for specific use
w = R0         -- scratch
temp1 = R1
tos  = R4      -- top of data stack value
ip = R6        -- interpretive pointer
ftable = R7    -- base regiser for flash token table
stack = R10    -- data stack pointer
rstack = R11   -- return stack pointer

-- Flash token table store some suitable constants in the beginning
rstackResetOffset = 0
stackResetOffset  = 4

-- | Primitive words for Cortex-M.
instance Primitive (IM ARMInstr) where
  exit     = popRStack ip
  execute  = insRec (mov ip (RegOp tos)) <>
             popStack tos
  swap     = insRec (mov w (RegOp tos)) <>
             insRec (ldr tos (RegIndOffset stack 0)) <>
             insRec (str w (RegIndOffset stack 0))
  drop     = popStack w
  dup      = pushStack tos
  over     = pushStack tos <>
             insRec (ldr tos (RegIndOffset stack 4))
  rto      = pushStack tos <>
             popRStack tos
  tor      = pushRStack tos <>
             popStack tos
  rfetch   = pushStack tos <>
             insRec (ldr tos (RegIndOffset rstack 0))
  fetch    = insRec (ldr tos (RegIndOffset tos 0))
  cfetch   = insRec (ldrb tos (RegIndOffset tos 0))
  store    = popStack w <>
             insRec (str w (RegIndOffset tos 0)) <>
             popStack tos
  cstore   = popStack w <>
             insRec (strb w (RegIndOffset tos 0)) <>
             popStack tos
  minus    = popStack w <>
             insRec (subs tos w (RegOp tos))
  plus     = binary adds
  and      = binary ands
  or       = binary orrs
  xor      = binary eors
  twoStar  = singleShift lsls
  twoSlash = singleShift asrs
  lshift   = multiShift  lsls
  rshift   = multiShift  lsrs
  zerop    = insRec (sub tos NoReg (Imm 1)) <>
             insRec (sbcs tos tos NoOperand)
  lt0      = insRec (add tos tos NoOperand)  <>
             insRec (sbcs tos tos NoOperand)
  constant = pushStack tos <>
             insRec (ldr tos (RegIndOffset w 0))
  umstar   = insRec (ldr w (RegIndOffset stack 0)) <>
             insRec (umull w tos w tos) <>
             insRec (str w (RegIndOffset stack 0))
  ummod    = mempty  -- TBD

colonToken tok = insRec $ Directive $ WORD [tok]

-- | Target primitives for Cortex-M
instance TargetPrimitive ARMInstr where
  wordToken sym = colonToken $ E.Identifier (nameMangle sym)
  literal val = colonToken (E.Identifier litSymbol) <> colonToken val
  docol = insRec $ bl (Mem $ E.Identifier docolSymbol)
  dohere dict = insRec (bl (Mem $ E.Identifier dohereSymbol)) <>
                insRec (Directive $ WORD [E.Identifier ramBaseSymbol + dict^.tdict.hereRAM])
  doconst e = insRec (bl (Mem $ E.Identifier doconstSymbol))
  next = insRec $ b (Mem $ E.Identifier nextSymbol)

  docolImpl = insLabRec docolSymbol (str ip (PreIndexed rstack 4)) <>
              insRec (mov ip (RegOp LR))
  hereImpl  = labRec dohereSymbol <>
              (pushStack tos) <>
              insRec (ldr tos (RegIndOffset LR 0))
  nextImpl  = insLabRec nextSymbol (ldrh w (PostIndexed ip 2)) <>
              insRec (ldr w (RegRegInd ftable w (OpLSL 2))) <>
              insRec (ldr PC (PostIndexed w 4))

  resetRStack = insRec (ldr rstack (RegIndOffset ftable rstackResetOffset))
  resetStack  = insRec (ldr  stack (RegIndOffset ftable  stackResetOffset))

  substNative word = case word^.name of
                       "ROT" -> word & doer.~(popStack w     <>
                                              popStack temp1 <>
                                              pushStack w    <>
                                              pushStack tos  <>
                                              insRec (mov tos (RegOp temp1)) <>
                                              next)
                       otherwise -> word


token lab = insRec $ Directive $ WORD lab

popStack = popXStack stack
popRStack = popXStack rstack
popXStack stk d = insRec $ ldr d (PostIndexed stk 4)

pushStack = pushXStack stack
pushRStack = pushXStack rstack
pushXStack stk r = insRec $ str r (PreIndexed stk (-4))

binary ins = popStack w <>
             insRec (ins tos w NoOperand)

singleShift ins = insRec (ins tos tos (Imm 1))

multiShift ins = popStack w <>
                 insRec (ins tos w (RegOp tos))

-- | Generate code for a dictionary for Cortex-M
-- codeGenerateCortexM :: (forall t. Dictionary (IM t)) -> ByteString
codeGenerateCortexM dict = emitCode $ codeGenerate Directive pad2 (bindCortexM dict)
