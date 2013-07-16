module Forth.Machine (MachineM, ForthLambda) where

import Control.Monad.State.Lazy

data Machine cell

type ForthLambda cell = MachineM cell ()

type MachineM cell = StateT (Machine cell) IO

