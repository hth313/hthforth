{- |

   Generic code generator.

-}

module Language.Forth.CodeGenerate (codeGenerate) where

import Data.Monoid
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.ByteString as B2
import Language.Forth.Dictionary
import Language.Forth.Word
import Translator.Expression
import Translator.Assembler.Generate

-- | Generalized Forth code generator
codeGenerate :: ([ByteString] -> a) -> Dictionary (IM a) -> (IM a)
codeGenerate ascii dict = visit (latest dict)  where
  visit Nothing = mempty
  visit (Just word) = visit (link word) <> generate word
  generate word = insRec (ascii [name word]) <>
                  labRec (B2.pack . mangle . B.unpack $ name word) <> doer word
  toExpr = Value . fromIntegral

-- Ensure the name is something the assembler accepts.
-- For a start we do nothing.
mangle = id
