{- |

   The monad used by the Forth interpreter.

-}

module Language.Forth.Interpreter.Monad (FMonad, FState(..), CV,
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
type FMonad cell = StateT (FState cell) (InputT IO)

-- Simpler way of specifying a cell value
type CV cell = CellVal cell (FMonad cell ())

-- | Interpreter state.
data FState cell = FState
  { stack  :: [CellVal cell (FMonad cell ())]  -- ^ Data stack
  , rstack :: [CellVal cell (FMonad cell ())]  -- ^ Return stack
  , ip     :: [FMonad cell ()]                 -- ^ Interpretive pointer
  , target :: Target cell
  , dict   :: Dictionary (FMonad cell ())      -- ^ Dictionary of Forth words
  , variables :: IntMap (DataField cell (FMonad cell ()))
  }
