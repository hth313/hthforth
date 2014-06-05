module Main (main) where

import Control.Monad
import Data.Int
import Language.Forth.Core
import Language.Forth.Machine
import Language.Forth.Target
import Util.Endian
import System.Console.Haskeline
import System.Directory
import System.Exit
import System.FilePath
import System.IO


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
      runInputT settings $ evalStateT (addNatives >> quit) (initialState target)
