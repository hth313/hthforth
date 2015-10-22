{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{- |

   Generic code generator.

-}

module Language.Forth.CrossCompiler.CodeGenerate (docolSymbol, doconstSymbol,
                                                  dohereSymbol, nextSymbol,
                                                  litSymbol, ramBaseSymbol,
                                                  codeGenerate, nameString, pad2) where

import Control.Lens
import Data.Bits
import Data.Char
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Monoid
import Data.Word
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as C2
import Language.Forth.Dictionary
import Language.Forth.TargetPrimitive
import Language.Forth.Word
import Translator.Expression
import Translator.Assembler.Directive
import Translator.Assembler.Generate
import Translator.Symbol


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
codeGenerate dir pad (dict, words) = header <> tokenTable <>
                                     natives <> (visit $ _latest dict)  where
  dataSize = dict^.hereRAM
  visit Nothing = mempty
  visit (Just word) = visit (_link word) <> generate (substNative word)
  generate word =
    let (bytes, chars) = nameString pad (C.unpack $ _name word)
        Just sym = word^.wordSymbol
        asciiRec | null chars = mempty
                 | otherwise = insRec $ dir $ ASCII [C.pack chars]
        tail | _name word == "EXIT" = labRec nextSymbol <> nextImpl
             | word^.wordKind == Native = next
             | otherwise = insEmpty
    in insRec (dir $ BYTE bytes) <> asciiRec <>
       labRec sym <> _doer word <> tail
  header = datafields <>  text
  natives = labRec docolSymbol   <> docolImpl <> next <>
           labRec doconstSymbol <> doconstImpl <> next <>
           labRec dohereSymbol  <> hereImpl <> next <>
           libCode
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

-- | Create a namestring, reversed and with bit 7 set in last character. Optionally
--   align the string.
nameString :: (Int -> Int) -> String -> ([Expr], String)
nameString pad s = align $ mark $ reverse s
  where mark (c:cs) = ([n], cs)
          where n = toExpr $ ord c .|. 0x80
        align (ns, ss) = (replicate (pad $ length s) (Value 0) ++ ns, ss)

-- | Pad to even 16-bit word
pad2 :: Int -> Int
pad2 n = n .&. 1
