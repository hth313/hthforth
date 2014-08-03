module Main (main) where

import Control.Monad
import Data.Int
import Translator.Assembler.Generate
import Language.Forth.Interpreter
import Language.Forth.Interpreter.Monad
import Language.Forth.Target
import Language.Forth.Target.CortexM
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

        codeGenerate = Just $ emitCode . codeGenerateCortexM
        targetDict   = Just $ dictionaryCortexM
        -- Empty
--        codeGenerate = Nothing
--        targetDict = Nothing :: Maybe (Dictionary (FM Int32 ()))

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
      runInputT settings $ evalStateT (initialVarStorage >> quit) (initialState target codeGenerate targetDict)
