module Language.Forth.Core (abort) where

import Language.Forth.Cell
import {-# SOURCE #-} Language.Forth.Machine

abort :: Cell cell => ForthLambda cell
