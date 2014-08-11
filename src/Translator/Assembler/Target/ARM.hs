{-# LANGUAGE OverloadedStrings #-}
{-

  ARM instruction set.

-}

module Translator.Assembler.Target.ARM (ARMInstr(..), CondExec(..), Reg(..), OpAdr(..),
                                        Update(..), Shift(..), Suffix(..),
                                        module Translator.Assembler.Directive,
                                        adc, adcs, add, adds, ands, asr, asrs, b, eors,
                                        ldr, ldrb, ldrh,
                                        lsl, lsls, lsr, lsrs, mov, movs, orrs,
                                        sbc, sbcs, str, strb, strh,
                                        sub, subs, umull) where

import Data.Char
import Data.Int
import Translator.Expression
import Translator.Assembler.Directive
import Translator.Assembler.InstructionSet


-- | ARM instruction set, with Thumb and Thumb2 as well as needed pseudo instructions.
data ARMInstr = ADD Update CondExec Suffix Reg Reg OpAdr Shift
              | ADC Update CondExec Suffix Reg Reg OpAdr Shift
              | AND Update CondExec Suffix Reg Reg OpAdr Shift
              | ASR Update CondExec Suffix Reg Reg OpAdr
              | B CondExec Suffix OpAdr
              | EOR Update CondExec Suffix Reg Reg OpAdr Shift
              | LDR    CondExec Suffix Reg OpAdr
              | LDRB   CondExec Suffix Reg OpAdr
              | LDRH   CondExec Suffix Reg OpAdr
              | LSL Update CondExec Suffix Reg Reg OpAdr
              | LSR Update CondExec Suffix Reg Reg OpAdr
              | MOV Update CondExec Suffix Reg OpAdr
              | ORR Update CondExec Suffix Reg Reg OpAdr Shift
              | SBC Update CondExec Suffix Reg Reg OpAdr Shift
              | STR    CondExec Suffix Reg OpAdr
              | STRB   CondExec Suffix Reg OpAdr
              | STRH   CondExec Suffix Reg OpAdr
              | SUB Update CondExec Suffix Reg Reg OpAdr Shift
              | UMULL CondExec Reg Reg Reg Reg
              | Directive GNUDirective

-- Constructors for common uses
adc   = i3 ADC U
adcs  = i3 ADC S
add   = i3 ADD U
adds  = i3 ADD S
ands  = i3 AND S
asr   = ASR U AL Any
asrs  = ASR S AL Any
b     = B AL Any
eors  = i3 EOR S
ldr   = LDR   AL Any
ldrb  = LDRB  AL Any
ldrh  = LDRH  AL Any
lsl   = LSL U AL Any
lsls  = LSL S AL Any
lsr   = LSR U AL Any
lsrs  = LSR S AL Any
mov   = MOV U AL Any
movs  = MOV S AL Any
orrs  = i3 ORR S
sbc   = i3 SBC U
sbcs  = i3 SBC S
str   = STR   AL Any
strb  = STRB  AL Any
strh  = STRH  AL Any
sub   = i3 SUB U
subs  = i3 SUB S
umull = UMULL AL

i3 ctor s x y z = ctor s AL Any x y z noShift

-- Update flags modifier
data Update = U | S

data CondExec = AL | EQ | NE | CS | CC | MI | PL | VS | VC | HI | LS | GE | LT | GT | LE
                deriving (Show, Eq)

data Reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 |
           SP | LR | PC | NoReg deriving (Eq, Show)

showReg = map toLower . show

data OpAdr = RegOp Reg | Mem Expr | NoOperand | Imm Expr | RegIndOffset Reg Expr |
             RegRegInd Reg Reg Shift | Cond CondExec | PreIndexed Reg Int | PostIndexed Reg Int
             deriving Eq

instance Show OpAdr where
    show (RegOp r)   = showReg r
    show (Mem mem)   = show mem
    show (Imm val)    = '#' : show val
    show (RegIndOffset r offset) = "[" ++ showReg r ++ ",#" ++ show offset ++ "]"
    show (RegRegInd r1 r2 (OpLSL 0)) = '[' : showReg r1 ++ "," ++ showReg r2 ++ "]"
    show (RegRegInd r1 r2 sh) = '[' : showReg r1 ++ "," ++ showReg r2 ++ "," ++ show sh ++ "]"
    show NoOperand = error "no operand"
    show Cond{} = error "cond"
    show (PreIndexed r n) = '[' : showReg r ++ ",#" ++ show n ++ "]!"
    show (PostIndexed r n) = '[' : showReg r ++ "],#" ++ show n

-- | Shifts in operands
data Shift = OpLSL Int | OpLSR Int | OpASR Int | OpROR Int | OpRRX
             deriving Eq

instance Show Shift where
    show (OpLSL n) = "lsl #" ++ show n
    show (OpLSR n) = "lsr #" ++ show n
    show (OpASR n) = "asr #" ++ show n
    show (OpROR n) = "ror #" ++ show n
    show OpRRX{}   = "rrx"

noShift = OpLSL 0

-- Thumb 2 word suffix
-- Any -  select what is appropriate
-- N   -  forced Narrow (Thumb 1)
-- A   -  ARM
-- W   -  Thumb2
data Suffix = Any | N | A | W deriving Show

instance InstructionSet ARMInstr where
  disassemble instr =
    let disasm (ADC f c q d s x sh)  = arith "adc" f c q d s x sh
        disasm (ADD f c q d s x sh)  = arith "add" f c q d s x sh
        disasm (AND f c q d s x sh)  = arith "and" f c q d s x sh
        disasm (ASR f c q d s x)     = (m (fl "asr" f)  c q, Just (showReg d :  showReg s : optop x))
        disasm (B c q d)             = (m "b"  c q, Just [show d])
        disasm (EOR f c q d s x sh)  = arith "eor" f c q d s x sh
        disasm (LDR c q d s)         = (m "ldr"   c q, Just [showReg d, op s])
        disasm (LDRB c q d s)        = (m "ldrb"  c q, Just [showReg d, op s])
        disasm (LDRH c q d s)        = (m "ldrh"  c q, Just [showReg d, op s])
        disasm (LSR f c q d s x)     = (m (fl "lsr" f) c q, Just (showReg d : showReg s : optop x))
        disasm (LSL f c q d s x)     = (m (fl "lsl" f) c q, Just (showReg d : showReg s : optop x))
        disasm (MOV S c q d s)       = (m "movs"  c q, Just [showReg d, op s])
        disasm (MOV U c q d s)       = (m "mov"   c q, Just [showReg d, op s])
        disasm (ORR f c q d s x sh)  = arith "orr" f c q d s x sh
        disasm (SBC f c q d s x sh)  = arith "sbc" f c q d s x sh
        disasm (STR c q d s)         = (m "str"   c q, Just [showReg d, op s])
        disasm (STRB c q d s)        = (m "strb"  c q, Just [showReg d, op s])
        disasm (STRH c q d s)        = (m "strh"  c q, Just [showReg d, op s])
        disasm (SUB f c q d s x sh)  = arith "sub" f c q d s x sh
        disasm (UMULL c r1 r2 r3 r4) = (m "umull" c Any, Just (map showReg [r1, r2, r3, r4]))

        disasm (Directive dir)       = disassemble dir

        arith mne f c q d NoReg NoOperand sh = (m (fl mne f) c q, Just (showReg d : shift sh))
        arith mne f c q d s NoOperand sh = (m (fl mne f) c q, Just (showReg d : showReg s : shift sh))
        arith mne f c q d s x sh         = (m (fl mne f) c q, Just (showReg d : showReg s : op x : shift sh))

        fl name U = name
        fl name S = name ++ "s"

        m name AL Any = name
        m name cc Any = name ++ show cc

        op = show

        shift (OpLSL 0) = []
        shift sh = [show sh]

        optop NoOperand = []
        optop x = [op x]

    in disasm instr
