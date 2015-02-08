module Language.Forth.TargetPrimitive (TargetPrimitive(..)) where

import Data.Monoid
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Expression (Expr)
import Translator.Symbol


-- | Extension primitives for a target.
class TargetPrimitive imt where
--  next :: a
  wordToken :: Symbol -> imt
  literal   :: Expr -> imt
  finish    :: imt -> imt
  docol     :: imt
  next      :: imt
  libNext   :: imt
  libDoCol  :: imt
  libRest   :: Monoid imt => imt
  libRest = mempty
