{-# LANGUAGE ExistentialQuantification, FlexibleContexts, RankNTypes #-}
{- |

   The Forth interpreter state and embedding it a monad transformer.

-}

module Language.Forth.Interpreter.State (FM, FState(..), CV,
                                         module Control.Monad.Trans.State ) where

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
import Language.Forth.Compiler
import Language.Forth.Dictionary
import Language.Forth.Primitive
import Language.Forth.Target
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Assembler.InstructionSet

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
