{- |

   The monad used by the Forth interpreter.

-}

module Language.Forth.Interpreter.Monad (FM, FState(..), CV,
                                         module Control.Monad.Trans.State) where

import Control.Monad.Trans.State
import Data.IntMap
import System.Console.Haskeline
import Language.Forth.CellVal
import Language.Forth.DataField
import Language.Forth.Dictionary
import Language.Forth.Target
import Language.Forth.WordId

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
  }
