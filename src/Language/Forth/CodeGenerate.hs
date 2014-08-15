{-# LANGUAGE OverloadedStrings #-}
{- |

   Generic code generator.

-}

module Language.Forth.CodeGenerate (codeGenerate, nameMangle) where

import Data.Bits
import Data.Char
import Data.Monoid
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as C2
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
                  labRec (C2.pack . nameMangle . C.unpack $ name word) <> doer word <> insEmpty
--  toExpr = Value . fromIntegral

-- Ensure the name is something the assembler accepts.
nameMangle :: String -> String
nameMangle s = prepend $ concatMap mangle s
  where mangle '@' = "_Fetch_"
        mangle '!' = "_Store_"
        mangle '+' = "_Plus_"
        mangle '-' = "_Minus_"
        mangle '*' = "_Star_"
        mangle '/' = "_Slash_"
        mangle '\'' = "_Tick_"
        mangle '=' = "_Equal_"
        mangle '>' = "_GreaterThan_"
        mangle '<' = "_LessThan_"
        mangle '"' = "_Quote_"
        mangle '\\' = "_BackSlash_"
        mangle c = [c]
        prepend s@(c:cs) | isDigit c = '_' : s
                         | otherwise = s
