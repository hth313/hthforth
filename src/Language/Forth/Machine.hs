{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes #-}
{- |

   The Forth interpreter state and embedding it a monad transformer.

-}

module Language.Forth.Machine (FM, FState(..), CV, module Control.Monad.Trans.State,
                               Compiler(..), Defining(..), DefiningWrapper(..)) where

import Control.Monad
import Control.Monad.Trans.State hiding (state)
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Vector.Storable.ByteString.Char8 as V (ByteString)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Vector (Vector)
import System.Console.Haskeline
import Language.Forth.Interpreter.Address
import Language.Forth.Interpreter.DataField
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.Target
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Assembler.InstructionSet
import Translator.Expression

-- Interpreter monad
type FM cell t = StateT (FState cell t) (InputT IO)

-- Simpler way of specifying a cell value
type CV cell t = CellVal cell (FM cell t ())

-- | Interpreter state.
data FState cell t = FState
  { stack  :: [CV cell t]               -- ^ Data stack
  , rstack :: [CV cell t]               -- ^ Return stack
  , ip     :: [FM cell t ()]            -- ^ Interpretive pointer
  , target :: Target cell
  , dict   :: Dictionary (FM cell t ()) -- ^ Dictionary of Forth words for interpreter
  , variables :: IntMap (DataField cell (FM cell t ()))
  , oldHandles :: [WordId]                 -- ^ Unused handles after reading source files
  , stringLiterals :: Map V.ByteString Addr
  , compilerFuns :: Compiler cell t (Defining (FM cell t ()))
  , defining :: Maybe (Defining (FM cell t ()))   -- ^ Collector when compiling
  , targetDict :: forall t1. InstructionSet t1 => Maybe (Dictionary (IM t1)) -- ^ Cross compiler dictionary
  }

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
