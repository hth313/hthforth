{-# LANGUAGE OverloadedStrings #-}
{- |

   Generic code generator.

-}

module Language.Forth.CrossCompiler.CodeGenerate (codeGenerate, nameMangle, nameString, pad2) where

import Data.Bits
import Data.Char
import Data.Monoid
import Data.Word
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import qualified Data.ByteString.Char8 as C2
import Language.Forth.Dictionary
import Language.Forth.Word
import Translator.Expression
import Translator.Assembler.Directive
import Translator.Assembler.Generate

-- | Generalized Forth code generator
codeGenerate :: (GNUDirective -> a) -> (Int -> Int) -> Dictionary (IM a) -> IM a
codeGenerate dir pad dict = visit (latest dict)  where
  visit Nothing = mempty
  visit (Just word) = visit (link word) <> generate word
  generate word =
    let (bytes, chars) = nameString pad (C.unpack $ name word)
        asciiRec | null chars = mempty
                 | otherwise = insRec $ dir $ ASCII [C.pack chars]
    in insRec (dir $ BYTE bytes) <>
       asciiRec <>
       labRec (C2.pack . nameMangle . C.unpack $ name word) <> doer word <> insEmpty

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
