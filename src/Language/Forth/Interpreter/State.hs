{-# LANGUAGE ExistentialQuantification #-}
{- |

   The Forth interpreter state and embedding it a monad transformer.

-}

module Language.Forth.Interpreter.State (FM, FState(..), TargetState(..), CV,
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

-- Interpreter monad
type FM cell = StateT (FState cell) (InputT IO)

-- Simpler way of specifying a cell value
type CV cell = CellVal cell (FM cell ())

-- | Interpreter state.
data FState cell = FState
  { stack  :: [CellVal cell (FM cell ())]  -- ^ Data stack
  , rstack :: [CellVal cell (FM cell ())]  -- ^ Return stack
  , ip     :: [FM cell ()]                 -- ^ Interpretive pointer
  , target :: Target cell
  , dict   :: Dictionary (FM cell ())      -- ^ Dictionary of Forth words
  , variables :: IntMap (DataField cell (FM cell ()))
  , oldHandles :: [WordId]                 -- ^ Unused handles after reading source files
  , stringLiterals :: Map V.ByteString Addr
  , defining :: Maybe (Defining cell (FM cell ())) -- ^ Collector when compiling
  , compiler :: Compiler cell (FM cell ())         -- ^ Compiler functions record
  , targetStates :: Map TargetKey TargetState      -- ^ targets
  }

-- | Representation of a target
data TargetState = forall t cc. Primitive cc t => TargetState
  { dumpTargetDict :: Dictionary t -> ByteString -- ^ generate code for target
  , targetDict :: Dictionary t                   -- ^ Cross compiler target dictionary
  }
