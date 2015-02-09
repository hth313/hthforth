module Language.Forth.TargetPrimitive (TargetPrimitive(..)) where

import Data.Monoid
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Expression (Expr)
import Translator.Symbol


-- | Extension primitives for a target.
class TargetPrimitive imt where
  -- | Compile a word/token as part of a colon definition
  wordToken :: Symbol -> imt

  -- | Compile a literal as part of a colon definition
  literal   :: Expr -> imt

  -- | Slice of code that will be first in a colon definition
  docol     :: imt

  -- | Normal way to end a primitive word  
  next      :: imt

  -- | Actual implementation of NEXT in the library
  libNext   :: imt

  -- | Actual implementation of DOCOL in the library
  libDoCol  :: imt

  -- | Any extra needed library code
  libRest   :: Monoid imt => imt
  libRest = mempty

  -- | Reset the stacks
  resetStack  :: imt               
  resetRStack :: imt               
