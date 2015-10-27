{-# LANGUAGE FlexibleContexts, RankNTypes, TemplateHaskell #-}
{- |

   The Forth interpreter state and embedding it a monad transformer.

-}

module Language.Forth.Machine (FM, FState(..), CV, module Control.Monad.Trans.State,
                               stack, rstack, ip, targetDict, dict, compilerFuns, variables,
                               Compiler(..), defining, compile, litComma, backpatch,
                               compileBranch, compileBranch0, recurse, closeDefining,
                               startDefining, abortDefining, setImmediate, reserveSpace,
                               Create(..), CreateStyle(..)) where

import Control.Lens
import Control.Monad
import Control.Monad.Trans.State hiding (state)
import Data.IntMap (IntMap)
import Data.Map (Map)
import qualified Data.Vector.Storable.ByteString.Char8 as V (ByteString)
import Data.ByteString.Lazy.Char8 (ByteString)
import System.Console.Haskeline
import Language.Forth.Interpreter.Address
import Language.Forth.Interpreter.DataField
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.TargetPrimitive
import Language.Forth.Target
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Assembler.InstructionSet
import Translator.Expression

-- Interpreter monad
type FM a = StateT (FState a) (InputT IO)

-- Simpler way of specifying a cell value
type CV a = CellVal (FM a ())

-- | Interpreter state.
data FState a = FState
  { _stack  :: [CV a]               -- ^ Data stack
  , _rstack :: [CV a]               -- ^ Return stack
  , _ip     :: [FM a ()]            -- ^ Interpretive pointer
  , _target :: Target
  , _dict   :: IDict (FM a ())      -- ^ Dictionary of Forth words for interpreter
  , _variables :: IntMap (DataField (FM a ()))
  , _oldHandles :: [WordId]         -- ^ Unused handles after reading source files
  , _stringLiterals :: Map V.ByteString Addr
  , _compilerFuns :: Compiler a
  , _targetDict :: forall t. (InstructionSet t, Primitive (IM t), TargetPrimitive t) =>
                   TDict t          -- ^ Cross compiler dictionary
  }

-- | Compiler primitives. This record keeps track of compiler primitives,
--   and is meant to be replaced when cross compiling to some other target.
data Compiler a = Compiler {
    _defining :: FState a -> Bool
    -- ^ Are we in a defining mode?
  , _compile :: CV a -> FState a -> Either String (FState a)
    -- ^ Compile a cell value to a colon definition
  , _litComma :: CV a -> FState a -> Either String (FState a)
    -- ^ Compile a cell value from the stack
  , _compileBranch :: FState a -> FState a
    -- ^ Compile a unconditional branch instruction
  , _compileBranch0 :: FState a -> FState a
    -- ^ Compile a conditional branch instruction
  , _backpatch :: CV a -> CV a -> FState a -> FState a
  , _recurse :: FState a -> FState a
    -- ^ Compile a recursive call back to the start of current definition
  , _startDefining ::Create (FM a ()) -> FState a -> FState a
    -- ^ Start defining a new word, bring the name and whether we are
    --   using CREATE to build a custom word or a colon definition.
  , _closeDefining :: FState a -> FState a
    -- ^ Add compiled word to active dictionary in the Forth state
  , _abortDefining :: FState a -> FState a
    -- ^ ABORT, stop whatever we are defining
  , _setImmediate :: FState a -> FState a
    -- ^ Set the immediate bit in the last defined word
  , _reserveSpace :: Cell -> FState a -> FState a
  -- ^ Reserve space in data memory
  }

-- | Data record used by startDefining
data Create a = Create {
    createName :: V.ByteString
  , finalizer :: [a] -> a
  , createStyle :: CreateStyle
}

data CreateStyle = DOCOL | CREATE | DOCONST Expr deriving Eq

makeLenses ''FState
makeLenses ''Compiler
