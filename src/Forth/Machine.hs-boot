module Forth.Machine (MachineM, ForthLambda) where

import Control.Monad.State.Lazy
import System.Console.Haskeline

data Machine cell

type ForthLambda cell = MachineM cell ()

type MachineM cell = StateT (Machine cell) (InputT IO)

