{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Input stream.

-}

module Forth.Input (nextWord) where

import Forth.Machine
import Data.Char

nextWord :: StateT (Machine cell) IO String
nextWord =
  StateT (\s ->
      let (word, inputStream') = break isSpace (dropWhile isSpace (inputStream s))
      in return (word, s { inputStream = inputStream' }))