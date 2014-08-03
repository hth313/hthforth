{-# LANGUAGE ExistentialQuantification #-}
{- |

   Primitive compiler functions.

-}

module Language.Forth.Compiler (Defining(..), DefiningWrapper(..), Compiler(..)) where

import Data.Vector (Vector)
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.Word
import Data.Vector.Storable.ByteString.Char8 (ByteString)

-- | The defining state. We collect words into a Vector together with
--   information about locations to change when we have collected all.
data Defining cell a = Defining
  { compileList :: Vector (DefiningWrapper cell a)
  , patchList :: [(Int, Int)]               -- ^ (loc, dest) list to patch
  , defineFinalizer :: [a] -> a
  , definingWord :: ForthWord a
  }
  | forall i. Primitive cell a => TargetBuilder
  { textSection :: Vector i                 -- ^ instructions
  }

-- | Wrapper for words being compile. This is used to keep track of branches
--   that are waiting to have their address fixed.
data DefiningWrapper cell a = WrapA a | WrapB ([a] -> a) | WrapRecurse

-- | Compiler primitives. This record keeps track of compiler primitives,
--   and is meant to be replaced when cross compiling to some other target.
data Compiler cell a = Compiler
  { compile :: CellVal cell a -> Defining cell a -> Defining cell a
    -- ^ Compile a cell value to a colon definition
  , litComma :: CellVal cell a -> Defining cell a -> Defining cell a
    -- ^ Compile a cell value from the stack
  , compileBranch :: CellVal cell a -> Defining cell a -> Defining cell a
    -- ^ Compile a unconditional branch instruction, not used by the interpreter
  , compileBranch0 :: CellVal cell a -> Defining cell a -> Defining cell a
    -- ^ Compile a conditional branch instruction, not used by the interpreter
  , recurse :: () -> Defining cell a -> Defining cell a
    -- ^ Compile a recursive call back to the start of current definition
  }
