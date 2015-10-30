{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TypeSynonymInstances #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-
  Forth target code generator for MSP430.

  Code generation is done by instantiating the Primitive type class which results
  from a cross compilation.

-}

module Language.Forth.Target.MSP430 (bindMSP430, codeGenerateMSP430) where

import Control.Lens
import qualified Data.ByteString.Char8 as C
import Data.Bits
import Data.Int
import Data.Monoid
import Data.ByteString.Lazy (ByteString)
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Language.Forth.CellVal
import Language.Forth.CrossCompiler.CodeGenerate
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.TargetPrimitive
import Language.Forth.Word
import Translator.Assembler.Generate
import Translator.Assembler.Target.MSP430
import Translator.Expression
import Translator.Symbol


-- | Bind a polymorphic target dictionary to be an MSP430 specific one
bindMSP430 :: (Dictionary (IM Instr430), IntMap (ForthWord (IM Instr430))) ->
              (Dictionary (IM Instr430), IntMap (ForthWord (IM Instr430)))
bindMSP430 = id

-- Forth machine registers
tos   = RegOp R4           -- top of stack, cached in register
w     = RegOp R5
stack = RegOp R6
ip    = RegOp R7
sp    = RegOp SP

-- Helpers to unwrap Forth machine registers when used in other addressing modes
indirect (RegOp reg) = Indirect reg
indirectInc (RegOp reg) = IndirectInc reg

-- Helpers to wrap instructions in an insRec
add  = binary ADD
and_ = binary AND
bis  = binary BIS
call = insRec . CALL
clrc = insRec CLRC
dec  = unary  DEC
decd = unary  DECD
inc  = unary  INC
incd = unary  INCD
inv  = unary  INV
jc   = insRec . JC
jnc  = insRec . JNC
jz   = insRec . JZ
jmp  = insRec . JMP
mov  = binary MOV
pop  = unary  POP
push = unary  PUSH
rla  = unary  RLA
rra  = unary  RRA
rrc  = unary  RRC
sub  = binary SUB
subc = binary SUBC
tst  = unary  TST
xor_ = binary XOR

binary ctor s op1 op2 = insRec $ ctor s op1 op2
unary ctor s op = insRec $ ctor s op

label = labRec . mkSymbol

-- | Primitive words for MSP430.
instance Primitive (IM Instr430) where
  exit     = pop W ip
  execute  = mov W tos ip <>
             popStack tos
  swap     = mov W tos w <>
             mov W (indirect stack) tos <>
             mov W w (indirect stack)
  drop     = popStack tos
  dup      = pushStack tos
  over     = mov W (indirect stack) w <>
             pushStack tos <>
             mov W w tos
  rto      = pushStack tos <>
             pop W tos
  tor      = push W tos <>
             popStack tos
  rfetch   = pushStack tos <>
             mov W (indirect sp) tos
  fetch    = mov W (indirect tos) tos
  cfetch   = mov B (indirect tos) tos
  store    = mov W (indirectInc stack) w <>
             mov W w (indirect tos) <>
             popStack tos
  cstore   = mov W (indirectInc stack) w <>
             mov B w (indirect tos) <>
             popStack tos
  plus     = add W (indirectInc stack) tos
  minus    = sub W (indirectInc stack) tos <>
             inv W tos <>
             inc W tos
  and      = and_ W (indirectInc stack) tos
  or       = bis  W (indirectInc stack) tos
  xor      = xor_ W (indirectInc stack) tos
  twoStar  = rla W tos
  twoSlash = rra W tos
  lshift   = multiShift 'l' rla
  rshift   = multiShift 'r' (\s r -> clrc <> rrc s r)
  zerop    = sub W (Imm 1) tos <>
             subc W tos tos
  lt0      = rla W tos <>
             subc W tos tos
  constant = pushStack tos <>
             mov W (indirect w) tos
  umstar   = mov W (indirect stack) (Absolute (Identifier "MPY")) <>
             mov W tos (Absolute (Identifier "OP2")) <>
             mov W (Absolute (Identifier "RESLO")) (indirect stack) <>
             mov W (Absolute (Identifier "RESHI")) tos
  ummod    = mempty  -- TBD

colonToken tok = insRec $ Directive $ WORD [tok]

instance TargetPrimitive Instr430 where
  cellValue e = insRec $ Directive $ LONG [e]
  wordToken (TargetToken _ sym) = colonToken (Identifier sym)
  literal val = colonToken (Identifier litSymbol) <> colonToken val
  labelOffset sym = colonToken $ Identifier sym - locationCounter
  docol   = call (Imm (Identifier docolSymbol))
  doconst e = call (Imm (Identifier doconstSymbol)) <>
              colonToken e
  dohere dict tt = (does tt, does)
    where does (TargetToken _ _) =
            call (Imm (Identifier dohereSymbol)) <>
            insRec (Directive $ WORD [Identifier ramBaseSymbol + dict^.tdict.hereRAM])
  next = jmp (Imm (Identifier nextSymbol))
  lit  = pushStack tos <>
         mov W (indirectInc ip) tos
  docolImpl   = mempty -- TBD
  doconstImpl = mempty -- TBD
  hereImpl    = mempty -- TBD
  nextImpl    = mempty -- TBD
  resetRStack = mempty -- TBD
  resetStack  = mempty -- TBD
--  reservedLabels _ = Set.fromList [mkSymbol (p:b) | p <- ['l', 'r'], b <- [shiftloop, shiftskip]]

-- Helper function for implementing LSHIFT and RSHIFT
multiShift t shift =
  popStack w    <>
  tst W tos     <>
  jz skip       <>
 label loop     <>
  shift W w     <>
 label skip     <>
  dec W tos     <>
  jnc loop      <>
  mov W w tos
    where loop = t : shiftloop
          skip = t : shiftskip

shiftloop = "shiftloop"
shiftskip = "shiftskip"

-- Pop data stack to given register
popStack r = mov W (indirectInc stack) r

-- Push given register to data stack. Pushing 'tos' effectively makes room
-- for pushing a new value on the stack (by moving it to 'tos').
-- MSP430 cannot predecrement, so used 'decd' to adjust data stack pointer.
pushStack r = decd W stack <>
              mov W r (indirect stack)

-- | Generate code for a dictionary for MSP430
-- codeGenerateMSP430 :: (forall t. Dictionary (IM t)) -> ByteString
codeGenerateMSP430 (dict, allwords) =
  emitCode $ codeGenerate Directive pad2 (bindMSP430 (dict, allwords))
