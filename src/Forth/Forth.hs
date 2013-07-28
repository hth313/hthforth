{-# LANGUAGE ScopedTypeVariables #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Main module.

-}

module Main (main) where

import Control.Monad.Trans
import Data.Int
import Forth.Cell
import Forth.Core
import Forth.Machine
import Forth.Target
import Util.Endian
import Control.Monad.CatchIO
import Control.Exception (IOException, AsyncException)
import System.Exit
import System.IO


main =
    let target = Target 4 4 1 LittleEndian :: Target Int32
    in do
      putStrLn "The Planet Pluto Forth, version 1.1.1"
      evalStateT (addNatives >> topLevel) (initialState target)

topLevel =
    let showExc e = liftIO $ hPutStrLn stderr (show e)
    in do
      quit `catches` [ Handler (\(e :: IOException) -> showExc e),
                       Handler (\(e :: AsyncException) -> showExc e),
                       Handler (\(e :: ForthException) -> showExc e) ]
      topLevel