{-# LANGUAGE ExistentialQuantification #-}
{- |

   The monad used by the Forth interpreter.

-}

module Language.Forth.Interpreter.Monad (FM, FState(..), CV,
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
data FState cell = forall ca cc. Primitive cc ca => FState
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
  , dumpTargetDict :: Maybe (Dictionary ca -> ByteString) -- ^ generate code for target
  , targetDict :: Maybe (Dictionary ca)            -- ^ Cross compiler target dictionary
  }
