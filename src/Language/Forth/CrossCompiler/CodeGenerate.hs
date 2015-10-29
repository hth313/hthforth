{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |

   Generic code generator.

-}

module Language.Forth.CrossCompiler.CodeGenerate (docolSymbol, doconstSymbol,
                                                  dohereSymbol, nextSymbol,
                                                  litSymbol, ramBaseSymbol,
                                                  codeGenerate, pad2) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Bits
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Monoid
import Data.Word
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.UTF8 as UTF8
import Language.Forth.Dictionary
import Language.Forth.TargetPrimitive
import Language.Forth.Word
import Translator.Expression
import Translator.Assembler.Directive
import Translator.Assembler.Generate
import Translator.Symbol


-- | Offset to previous word (backwards) by this number of bits
linkSize = 20

-- | Some predefined symbols for specific purposes in a target
docolSymbol, doconstSymbol, dohereSymbol, nextSymbol, litSymbol, ramBaseSymbol :: Symbol
docolSymbol   = "DOCOL"
doconstSymbol = "DOCONST"
dohereSymbol  = "DOHERE"
nextSymbol    = "NEXT"
litSymbol     = "LIT"
ramBaseSymbol = "RAMBASE"
tokenSymbol   = "TokenTable"

-- | Generalized Forth code generator
codeGenerate ::  forall t. TargetPrimitive t => (GNUDirective -> t) -> (Int -> Int) -> (Dictionary (IM t), IntMap (ForthWord (IM t))) -> IM t
codeGenerate dir pad (dict, words) =    header
                                     <> tokenTable
                                     <> natives
                                     <> (evalState (visit $ _latest dict) (Value 0))  where
  dataSize = dict^.hereRAM
  visit Nothing = return mempty
  visit (Just word) = liftM2 (<>) (visit (_link word)) (state $ generate (substNative word))
  generate word prevLabel =
    let (bytes, chars) = nameString pad name
        name = C.take namelen fullname
        namelen = min maxNameLen (C.length fullname)
        fullname = UTF8.fromString (_name word)
        Just sym = word^.wordSymbol
        alignment | null bytes = mempty
                  | otherwise = insRec (dir $ BYTE bytes)
        asciiRec | C.null chars = mempty
                 | otherwise = insRec $ dir $ ASCII [chars]
        thisLabel = Identifier sym
        status = ((thisLabel - prevLabel) `shiftLeft` linkSize)
                 .|. (Value $ fromIntegral namelen)
                 .|. (Value $ foldl (.|.) 0 (map flagval (word^.wordFlags)))
          where flagval Immediate   = 1 `shiftL` 5
                flagval CompileOnly = 1 `shiftL` 6
                flagval _           = 0
        compileXT = Value 0
        tail | _name word == exitName = labRec nextSymbol <> nextImpl
             | word^.wordKind == Native = next
             | otherwise = mempty
    in (   alignment
        <> asciiRec                    -- name field
        <> cellValue status            -- link, name length and flags
        <> cellValue compileXT
        <> labRec sym <> _doer word    -- XT points here
        <> tail,
        thisLabel)
  header = datafields <>  text
  natives = labRec docolSymbol   <> docolImpl   <> next <>
            labRec doconstSymbol <> doconstImpl <> next <>
            labRec dohereSymbol  <> hereImpl    <> next <>
            libCode
  nameString pad name = (replicate (pad $ C.length name) 0, name)
  tokenTable = case tokenTableLine of
                 Just entry ->
                   let tt (n, (m, word)) =
                         let fillers = mconcat $ replicate (m - n) (entry $ Value 0)
                             Just sym = word^.wordSymbol
                         in fillers <> entry (Identifier sym)
                   in labRec tokenSymbol <> mconcat (map tt $ zip [0..] (IntMap.assocs words))
                 otherwise -> mempty

  datafields | dataSize > 0 = insRec (dir $ SECTION "datafields" "w") <>
                              labRec ramBaseSymbol <>
                              insRec (dir $ FILL [dataSize])
             | otherwise = mempty
  text = insRec (dir $ TEXT Nothing)

toExpr = Value . fromIntegral

-- | Pad to even 16-bit word
pad2 :: Int -> Int
pad2 n = n .&. 1
