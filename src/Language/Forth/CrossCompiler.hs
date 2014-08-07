{-# LANGUAGE ScopedTypeVariables #-}
{- |

   The Forth cross compiler.

   Using the Forth interpreter, replace the compiler primitives with variants
   that can compile to a real target. Cross compile by using the interpreter,
   but output to another dictionary based on the Primitive typeclass.
   At this point, we do not specify a particular target.
   When done, we take the created dictionary and instantiate it for
   a particular target and use it to generate target specific output.
   In reality, we generate assembler meant to be fed into a suitable
   cross assembler to get object code for the target.

-}

module Language.Forth.CrossCompiler (targetOutput) where

import Control.Applicative
import Control.Monad.IO.Class
import qualified Data.Map as Map
import Translator.Assembler.Generate
import Translator.Assembler.Target.ARM
import Language.Forth.Cell
import Language.Forth.CodeGenerate
import Language.Forth.Dictionary
import Language.Forth.Interpreter.Monad
import Language.Forth.Target (TargetKey)
import Language.Forth.Word
import qualified Data.ByteString.Lazy.Char8 as C
import System.IO (stdout)

targetOutput :: Cell cell => TargetKey -> FM cell ()
targetOutput k =
  let dump s = case Map.lookup k (targetStates s) of
          Just (TargetState { dumpTargetDict = dumpTargetDict, targetDict = targetDict })  ->
            dumpTargetDict targetDict
          Nothing -> C.empty
  in do
    text <- gets dump
    liftIO $ C.hPut stdout text
