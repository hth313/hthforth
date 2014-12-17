{-# LANGUAGE MultiParamTypeClasses #-}
{- |

   Primitive compiler functions.

-}

module Language.Forth.Compiler (Defining(..), DefiningWrapper(..), Compiler(..)) where

import Data.Vector (Vector)
import Language.Forth.Cell
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.Word
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import {-# SOURCE #-} Language.Forth.Interpreter.State
import Translator.Expression

-- | The defining state for the interpreter.
--   We collect words into a Vector together with information about locations
--   to change when we have collected all.
data Defining a = Defining {
    compileList :: Vector (DefiningWrapper a)
  , patchList :: [(Int, Int)]               -- ^ (loc, dest) list to patch
  , defineFinalizer :: [a] -> a
  , definingWord :: ForthWord a
  }
  | TargetDefining {
    tcompileList :: Vector Expr
}

-- | Wrapper for words being compile. This is used to keep track of branches
--   that are waiting to have their address fixed.
data DefiningWrapper a = WrapA a | WrapB ([a] -> a) | WrapRecurse

-- | Compiler primitives. This record keeps track of compiler primitives,
--   and is meant to be replaced when cross compiling to some other target.
data Compiler cell t d = Compiler {
    compile :: CV cell t -> d -> d
    -- ^ Compile a cell value to a colon definition
  , litComma :: CV cell t -> d -> d
    -- ^ Compile a cell value from the stack
  , compileBranch :: CellVal cell t -> d -> d
    -- ^ Compile a unconditional branch instruction, not used by the interpreter
  , compileBranch0 :: CellVal cell t -> d -> d
    -- ^ Compile a conditional branch instruction, not used by the interpreter
  , recurse :: d -> d
    -- ^ Compile a recursive call back to the start of current definition
  , addCompiledWord :: d -> FState cell t -> FState cell t
    -- ^ Add compiled word to active dictionary in the Forth state
  }