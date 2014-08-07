{-# LANGUAGE OverloadedStrings #-}
{-

  MSP430 instruction set.

-}

module Translator.Assembler.Target.MSP430 (Instr430(..), Suffix(..), Reg(..), Op(..)) where

import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C
import Translator.Expression
import Translator.Symbol
import Translator.Assembler.InstructionSet

data Instr430 = ADD  Suffix Op Op
              | ADDC Suffix Op Op
              | AND  Suffix Op Op
              | BIS  Suffix Op Op
              | CLRC
              | DEC  Suffix Op
              | DECD Suffix Op
              | INC  Suffix Op
              | INCD Suffix Op
              | INV  Suffix Op
              | JC   Lab
              | JNC  Lab
              | JZ   Lab
              | MOV  Suffix Op Op
              | POP  Suffix Op
              | PUSH Suffix Op
              | RLA  Suffix Op
              | RRA  Suffix Op
              | RRC  Suffix Op
              | SUB  Suffix Op Op
              | SUBC Suffix Op Op
              | TST  Suffix Op
              | XOR  Suffix Op Op
              -- pseudo instructions
              | ASCII [ByteString]

type Lab = String

data Suffix = B | W deriving (Eq, Show)

data Reg = PC | SP | SR | CG1 | CG2 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15
           deriving (Eq, Show)

data Op = RegOp Reg | Indexed Expr Reg | Symbolic Expr | Absolute Expr | Indirect Reg | IndirectInc Reg
        | Imm Expr

instance Show Op where
  show (RegOp reg)       = show reg
  show (Indexed n reg)   = show n ++ "(" ++ show reg ++ ")"
  show (Symbolic n)      = show n
  show (Absolute n)      = '&' : show n
  show (Indirect reg)    = '@' : show reg
  show (IndirectInc reg) = '@' : (show reg ++ "+")
  show (Imm n)           = '#' : show n

instance InstructionSet Instr430 where
  disassemble instr =
    let disasm (ADD s op1 op2)  = dis "add"  s op1 (Just op2)
        disasm (ADDC s op1 op2) = dis "addc" s op1 (Just op2)
        disasm (AND  s op1 op2) = dis "and"  s op1 (Just op2)
        disasm (BIS  s op1 op2) = dis "bis"  s op1 (Just op2)
        disasm CLRC             = ("clrc", Nothing)
        disasm (DEC  s op)      = dis "dec"  s op  Nothing
        disasm (DECD s op)      = dis "decd" s op  Nothing
        disasm (INC  s op)      = dis "inc"  s op  Nothing
        disasm (INCD s op)      = dis "incd" s op  Nothing
        disasm (INV  s op)      = dis "inv"  s op  Nothing
        disasm (JC   lab)       = ("jc",  Just [lab])
        disasm (JNC  lab)       = ("jnc", Just [lab])
        disasm (JZ   lab)       = ("jz",  Just [lab])
        disasm (MOV  s op1 op2) = dis "mov"  s op1 (Just op2)
        disasm (POP  s op)      = dis "pop"  s op  Nothing
        disasm (PUSH s op)      = dis "push" s op  Nothing
        disasm (RLA  s op)      = dis "rla"  s op  Nothing
        disasm (RRA  s op)      = dis "rra"  s op  Nothing
        disasm (RRC  s op)      = dis "rrc"  s op  Nothing
        disasm (SUB  s op1 op2) = dis "sub"  s op1 (Just op2)
        disasm (SUBC s op1 op2) = dis "subc" s op1 (Just op2)
        disasm (TST  s op)      = dis "tst"  s op  Nothing
        disasm (XOR  s op1 op2) = dis "xor"  s op1 (Just op2)

        disasm (ASCII strings)  = (".ascii", Just (map (show . C.unpack) strings))

        dis mne s op  Nothing             = (n mne s, Just [show op])
        dis mne s op1 (Just (Indirect r)) = (n mne s, Just [show op1, show (Indexed 0 r)])
        dis mne s op1 (Just op2)          = (n mne s, Just (map show [op1, op2]))

        n mne W = mne
        n mne B = mne ++ ".b"

    in disasm instr
