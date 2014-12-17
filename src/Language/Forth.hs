module Main (main) where

import Control.Monad
import Data.Int
import qualified Data.Map as Map
import Translator.Assembler.Generate
import Language.Forth.Interpreter
import Language.Forth.Interpreter.State
import Language.Forth.Target
import Language.Forth.Target.CortexM
import Language.Forth.Target.MSP430
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
      putStrLn "Forth-2012 System (Subset) with Environmental Restrictions"
      putStrLn "Providing PARSE and WITHIN from the Core Extensions word set"
      -- Configure and run Haskeline
      adir <- getAppUserDataDirectory name
      createDirectoryIfMissing False adir
      let history = combine adir (name ++ "history")
      let settings = Settings { complete = noCompletion,
                                historyFile = Just history,
                                autoAddHistory = True }
      runInputT settings $ evalStateT (initialVarStorage >> quit) (initialState target)
