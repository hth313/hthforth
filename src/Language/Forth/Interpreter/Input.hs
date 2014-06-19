{-|

  Handle the input stream.

-}

module Language.Forth.Input (nextWord) where

import Language.Forth.Interpreter.Monad
import Data.Char
import System.IO

-- | Obtain the next word from the input source
nextWord :: StateT (Machine cell) IO String
nextWord = do
  input <- readMachine inputStream
  input' <- case dropWhile isSpace input of
                    "" -> liftIO readLine
                    input -> return input
  let (word, inputStream') = break isSpace input'
  update (\s -> s { inputStream = inputStream' })
  return word

-- | Read a fresh line of input from stdin
readLine = do
  putStr "ok " >> hFlush stdout
  text <- getLine
  case dropWhile isSpace text of
    "" -> readLine      -- try again
    text -> return text
