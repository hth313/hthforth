{-# LANGUAGE OverloadedStrings #-}
{-
  Generic code generation support.
  This is meant for textual output of assembler code, without doing any transformations
  or optimizations, just plain output.
-}

module Translator.Assembler.Generate (IM, insRec, insLabRec, labRec, emitCode) where

import Data.Monoid
import Data.List
import Translator.Symbol
import qualified Data.DList as DL
import Data.DList (DList)
import System.IO
import qualified Data.ByteString.Char8 as C
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.Char8
import Translator.Assembler.InstructionSet


-- | Instruction monoid we code generate over.
type IM a = DList (InsRec a)

-- | Instruction records. These are typically assembler instructions that
--   can have either a label (symbol), assembler instruction, or both.
data InsRec i = InsRec i | InsLabRec Symbol i | LabRec Symbol

-- | Instruction record elevated into the instruction monoid
insRec :: a -> IM a
insRec = DL.singleton . InsRec

insLabRec :: Symbol -> a -> IM a
insLabRec sym i = DL.singleton (InsLabRec sym i)

labRec :: Symbol -> IM a
labRec = DL.singleton . LabRec

spaces14 = replicate 14 ' '

nl = fromByteString "\n"

emitRec (InsRec i) = fromByteString "              " <> ppi (disassemble i) <> nl
emitRec (LabRec lab) = fromByteString lab <> fromByteString ":" <> nl
emitRec (InsLabRec lab i) = fromByteString lab <> fromByteString ":" <>
                            fromString (drop (1 + fromIntegral (C.length lab)) spaces14) <>
                            ppi (disassemble i) <> nl
ppi (mne, Nothing) = fromString mne
ppi (mne, Just ops) = fromString mne <> fromString (drop (length mne) spaces14) <>
                      fromString (intercalate "," ops)

emitCode code = toLazyByteString (mconcat $ map emitRec $ DL.toList code)
