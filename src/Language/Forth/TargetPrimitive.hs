{-# LANGUAGE FlexibleContexts #-}
module Language.Forth.TargetPrimitive (TargetPrimitive(..)) where

import Data.Monoid
import Language.Forth.Dictionary
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Expression (Expr)
import Translator.Symbol


-- | Extension primitives for a target.
class TargetPrimitive t where
  -- | Compile a word/token as part of a colon definition
  wordToken :: Symbol -> IM t

  -- | Compile a literal as part of a colon definition
  literal :: Expr -> IM t

  -- | Slice of code that will be first in a colon definition
  docol :: IM t

  -- | Slice of code to compile a datafield pointer
  dohere :: TDict t -> IM t

  -- | Slice of code to compile a constant as a separate word
  doconst :: Expr -> IM t

  -- | Normal way to end a primitive word
  next :: IM t

  -- | Actual implementation of NEXT in the library
  nextImpl :: IM t

  -- | Actual implementation of DOCOL in the library
  docolImpl :: IM t

  -- | Actual implementation of DOHERE in the library
  hereImpl :: IM t

  -- | Any extra needed library code
  libCode :: Monoid (IM t) => IM t
  libCode = mempty

  -- | Reset the stacks
  resetStack  :: IM t
  resetRStack :: IM t
