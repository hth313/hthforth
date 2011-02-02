{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Input stream.

-}

module Forth.Input (nextWord) where

import Forth.Machine
import Data.Char
import System.IO

nextWord :: StateT (Machine cell) IO String
nextWord = do
  input <- readMachine inputStream
  input' <- case dropWhile isSpace input of
                    "" -> liftIO readLine
                    input -> return input
  let (word, inputStream') = break isSpace input'

  update (\s -> s { inputStream = inputStream' })
  return word

readLine = do
  putStr "ok "
  hFlush stdout
  text <- getLine
  case dropWhile isSpace text of
    "" -> readLine
    text -> return text