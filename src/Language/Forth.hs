module Main (main) where

import Control.Monad
import Data.Int
import Language.Forth.Interpreter
import Language.Forth.Interpreter.Monad
import Language.Forth.Target
import Util.Endian
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath
import System.IO

import Language.Forth.Primitive
import Language.Forth.Cell

main :: IO ()
main =
    let target = Target 4 4 1 LittleEndian :: Target Int32
        name = "hthforth"
    in do
      putStrLn $ name ++ ", version 1.1.1"
      putStrLn "A Forth-2012 System Subset - under development"
      -- Configure and run Haskeline
      adir <- getAppUserDataDirectory name
      createDirectoryIfMissing False adir
      let history = combine adir (name ++ "history")
      let settings = Settings { complete = noCompletion,
                                historyFile = Just history,
                                autoAddHistory = True }
      runInputT settings $ evalStateT (initialVarStorage >> quit) (initialState target)
