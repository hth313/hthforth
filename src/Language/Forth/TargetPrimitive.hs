{-# LANGUAGE FlexibleContexts #-}
module Language.Forth.TargetPrimitive (TargetPrimitive(..)) where

import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Expression (Expr)
import Translator.Symbol


-- | Extension primitives for a target.
class TargetPrimitive t where
  -- | Compile given cell value (used when building header)
  cellValue :: Expr -> IM t

  -- | Compile an address or token as part of a colon definition.
  wordToken :: TargetToken -> IM t

  -- | Compile a literal as part of a colon definition
  literal :: Expr -> IM t

  -- | Compile a relative branch offset, usually inserted after a BRANCH
  --   of BRANCH0.
  labelOffset :: Symbol -> IM t

  -- | Slice of code that will be first in a colon definition
  docol :: IM t

  -- | Slice of code that is first on a constant definition
  doconst :: Expr -> IM t

  -- | Slice of code to compile a datafield pointer
  dohere :: TDict t -> IM t

  -- | Normal way to end a primitive word
  next :: IM t

  -- | Word that will pick up the following embedded literal value
  --   and push it on the stack.
  lit :: IM t

  -- | Actual implementation of NEXT in the library
  nextImpl :: IM t

  -- | Actual implementation of DOCOL in the library
  docolImpl :: IM t

  -- | Actual implementation of DOCONST in the library
  doconstImpl :: IM t

  -- | Actual implementation of DOHERE in the library
  hereImpl :: IM t

  -- | Any extra needed library code
  libCode :: Monoid (IM t) => IM t
  libCode = mempty

  -- | Reset the stacks
  resetStack  :: IM t
  resetRStack :: IM t

  -- | Branch instructions
  branch  :: IM t
  branch0 :: IM t

  -- | Loop instructions
  loop :: IM t
  plusLoop :: IM t

  -- | Optionally substitute a word with a native implementation
  substNative :: ForthWord (IM t) -> ForthWord (IM t)
  substNative = id

  -- | Build a token table. Given a table index, generate a line for the table.
  tokenTableLine :: Maybe (Expr -> IM t)
  tokenTableLine = Nothing
