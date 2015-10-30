{-# LANGUAGE PatternGuards #-}
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
import Data.IntMap (IntMap)
import Language.Forth.CellVal
import Language.Forth.CrossCompiler.CodeGenerate
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.TargetPrimitive
import Language.Forth.Word
import qualified Translator.Expression as E
import Translator.Assembler.Generate
import Translator.Assembler.Target.ARM
import Translator.Symbol
import Prelude hiding (EQ)


-- | Bind a polymorphic target dictionary to be an CortexM specific one
bindCortexM :: (Dictionary (IM ARMInstr), IntMap (ForthWord (IM ARMInstr))) ->
               (Dictionary (IM ARMInstr), IntMap (ForthWord (IM ARMInstr)))
bindCortexM = id

-- Registers assigned for specific use
w = R0         -- work
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
  cellValue e = insRec $ Directive $ LONG [e]
  wordToken (TargetToken wid _) = colonToken $ E.Value $ fromIntegral $ unWordId wid
  literal val = colonToken (E.Identifier litSymbol) <> colonToken val
  labelOffset sym = colonToken $ E.Identifier sym - E.locationCounter
  docol = insRec $ bl (Mem $ E.Identifier docolSymbol)
  dohere dict tt = (does tt, does)
    where does (TargetToken wid _) =
            insRec (bl (Mem $ E.Identifier dohereSymbol)) <>
            insRec (Directive $ LONG [E.Identifier ramBaseSymbol + dict^.tdict.hereRAM]) <>
            colonToken (E.Value $ fromIntegral $ unWordId wid)
  doconst e = insRec (bl (Mem $ E.Identifier doconstSymbol)) <>
              token [e]
  next    = insRec $ b (Mem $ E.Identifier nextSymbol)
  lit = pushStack tos <>
        insRec (ldr tos (PostIndexed ip 4))

  docolImpl   = pushRStack ip <>
                insRec (mov ip (RegOp LR))
  doconstImpl = pushStack tos <>
                insRec (ldr tos (RegIndOffset LR 0))
  hereImpl    = pushStack tos <>
                insRec (ldr tos (RegIndOffset LR 0)) <>
                insRec (ldrh w (RegIndOffset LR 4)) <>
                insRec (ldr PC (RegRegInd ftable w (OpLSL 2)))
  nextImpl    = insRec (ldrh w (PostIndexed ip 2)) <>
                insRec (ldr PC (RegRegInd ftable w (OpLSL 2)))

  resetRStack = insRec (ldr rstack (RegIndOffset ftable rstackResetOffset))
  resetStack  = insRec (ldr  stack (RegIndOffset ftable  stackResetOffset))

  branch  = insRec (ldrsh w (RegIndOffset ip 0)) <>
            insRec (adds ip ip (RegOp w))
  branch0 = insRec (teq tos (Imm 0)) <>
            popStack tos <>
            insRec (B EQ Any (Mem $ E.Identifier "BRANCH")) <>
            insRec (adds ip ip (Imm 2))
  loop = insRec (ldr w (RegIndOffset rstack 4)) <>
         insRec (adds w w (Imm 1)) <>
         insRec (str w (RegIndOffset rstack 4)) <>
         insRec (ldr temp1 (RegIndOffset rstack 0)) <>
         insRec (cmp w (RegOp temp1) noShift) <>
         insRec (B NE Any (Mem $ E.Identifier "BRANCH")) <>
         labRec loopLeave <>
         insRec (adds ip ip (Imm 2)) <>
         insRec (ldr w (PostIndexed rstack 8))
  plusLoop = insRec (ldr w (RegIndOffset rstack 4)) <>
             insRec (adds w w (RegOp tos)) <>
             popStack tos <>
             insRec (B CS Any (Mem $ E.Identifier loopLeave)) <>
             insRec (ldr temp1 (RegIndOffset rstack 0)) <>
             insRec (cmp w (RegOp temp1) noShift) <>
             insRec (B CC Any (Mem $ E.Identifier loopLeave)) <>
             insRec (str w (RegIndOffset rstack 4)) <>
             insRec (B AL Any (Mem $ E.Identifier "BRANCH"))
  leave = insRec (ldr w (PostIndexed rstack 8))

  substNative word | Just n <- word^.name =
                        case n of
                          "ROT" -> repl (popStack w     <>
                                         popStack temp1 <>
                                         pushStack w    <>
                                         pushStack tos  <>
                                         insRec (mov tos (RegOp temp1)) <>
                                         next)
                          "(DO)" -> repl (popStack w     <>
                                          pushRStack w   <>
                                          pushRStack tos <>
                                          popStack tos <>
                                          next)
                          otherwise -> word
                   | otherwise = word
     where repl code = word & doer.~code

  tokenTableLine = Just $ \n -> insRec $ Directive $ LONG [n]

loopLeave = mkSymbol "LoopLeave"

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
--codeGenerateCortexM :: (forall t. (Dictionary (IM t), IntMap (ForthWord (IM t)))) -> ByteString
codeGenerateCortexM (dict, allwords) =
  emitCode $ codeGenerate Directive pad2 (bindCortexM (dict, allwords))
