{- |

   The monad used by the Forth interpreter.

-}

module Language.Forth.Interpreter.Monad (FM, FState(..), CV, Defining(..),
                                         DefiningWrapper(..), unWrapA,
                                         module Control.Monad.Trans.State ) where

import Control.Monad.Trans.State hiding (state)
import Data.IntMap
import Data.Vector (Vector)
import System.Console.Haskeline
import Language.Forth.Interpreter.DataField
import Language.Forth.CellVal
import Language.Forth.Dictionary
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
  , defining :: Maybe (Defining cell)      -- ^ Collector when compiling
  }

-- | The defining state. We collect words into a Vector together with
--   information about locations to change when we have collected all.
data Defining cell = Defining
  { compileList :: Vector (DefiningWrapper cell)
  , patchList :: [(Int, Int)]               -- ^ (loc, dest) list to patch
  , definingWord :: ForthWord (FM cell ())
  }

-- | Wrapper for words being compile. This is used to keep track of branches
--   that are waiting to have their address fixed.
data DefiningWrapper cell = WrapA (FM cell ()) | WrapB ([FM cell ()] -> FM cell ())

unWrapA (WrapA a) = a
