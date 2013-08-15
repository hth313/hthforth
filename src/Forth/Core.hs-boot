module Forth.Core (abort) where

import Forth.Cell
import {-# SOURCE #-} Forth.Machine

abort :: Cell cell => ForthLambda cell
