module Language.Forth.Interpreter.State (CV, FState) where

import Control.Monad.Trans.State (StateT(..))
import Language.Forth.CellVal (CellVal(..))
import System.Console.Haskeline (InputT(..))

type FM cell t = StateT (FState cell t) (InputT IO)
type CV cell t = CellVal cell (FM cell t ())

data FState cell t
