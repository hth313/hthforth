{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, OverloadedStrings #-}
{- |

   The Forth cross compiler.

   The cross compiler is based on recompiling with some compiler words
   replaced by alternatives that write to a target dictionary.
   This dictionary is type polymorphic and can be bound by any target
   to generate output.
   Here we simply apply the target specific dictionary dumper to the
   polymorphic target dictionary to get the output.
   For the moment, we generate assembler meant to be fed into a suitable
   cross assembler to get object code for the target.

-}

module Language.Forth.CrossCompiler (crossCompiler) where

import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Vector as V
import Language.Forth.CellVal
import Language.Forth.Compiler
import Language.Forth.Dictionary
import Language.Forth.Interpreter.State
import Language.Forth.Target (TargetKey)
import Translator.Assembler.Generate (IM)
import Translator.Expression
import qualified Data.ByteString.Lazy.Char8 as C

-- | The cross compiler
crossCompiler = Compiler compile litComma compileBranch compileBranch0 recurse closeDefining  where
  compile val = addWord [cellToExpr val]
  litComma val = addWord [Identifier "LIT", cellToExpr val]
  compileBranch = compileBranch0
  compileBranch0 val def = def
  recurse def = def
  closeDefining (TargetDefining list) s =
    s { defining = Nothing, targetDict = addTargetWord list <$> targetDict s }

  addWord vs (TargetDefining list) = TargetDefining (foldl V.snoc list vs) -- V.concat list (V.fromList vs))
  addTargetWord _ s =  s