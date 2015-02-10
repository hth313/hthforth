{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts, RankNTypes, OverloadedStrings #-}
{- |

   The Forth cross compiler.

   The cross compiler is based on recompiling with some compiler words
   replaced by alternatives that write to a target dictionary.
   This dictionary is type polymorphic and can be bound by any target
   to generate output.
   For the moment, we generate assembler meant to be fed into a suitable
   cross assembler to get object code for the target.

-}

module Language.Forth.CrossCompiler (crossCompiler, targetDictionary, arbitraryTargetDict) where

import Control.Applicative
import Control.Lens
import qualified Data.Map as Map
import qualified Data.Vector as V
import Data.Maybe
import Data.Monoid
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Machine
import Language.Forth.Primitive
import Language.Forth.TargetPrimitive
import Language.Forth.Target (TargetKey)
import Language.Forth.Word
import Translator.Assembler.InstructionSet
import Translator.Assembler.Generate (IM)
import qualified Data.ByteString.Lazy.Char8 as C
import Translator.Expression

-- These imports (minor kludge) are for defining below binding to a specific target,
-- as I have yet to find a way to avoid it.
import Translator.Assembler.Target.ARM (ARMInstr)
import Language.Forth.Target.CortexM ()

-- | The cross compiler
crossCompiler = Compiler defining compile litComma compileBranch compileBranch0 recurse startDefining
                         closeDefining abortDefining setImmediate reserveSpace where
  defining = isJust . _tdefining . arbitraryTargetDict
  compile (XT _ _ (Just sym)) = addTokens $ wordToken sym
  compile val@Val{} = litComma val
  litComma val = addTokens $ literal $ cellToExpr val
  compileBranch = compileBranch0
  compileBranch0 val s = s
  recurse s = s
  abortDefining s = s { _targetDict = abortDefining1 $ _targetDict s }
    where abortDefining1 dict = dict & tdefining.~Nothing
  setImmediate s = s
  startDefining Create{..} s = s { _targetDict = startDefining1 $ _targetDict s }
    where startDefining1 dict = f $ dict { _tdefining = Just (TDefining createName doer) }
            where (f, doer) | usingCreate = (closeDefining1, dohere dict)
                            | otherwise   = (id, docol)
  closeDefining s = s { _targetDict = closeDefining1 $ _targetDict s }
  closeDefining1 dict = dict & tdefining.~Nothing & tdict.latest.~Just newWord
    where newWord = ForthWord name False (_latest $ _tdict dict) targetColonWordId
                    (_tcompileList (fromJust $ _tdefining dict))
          name = _wordName (fromJust $ _tdefining dict) 
  reserveSpace n s = s { _targetDict = targetAllot (fromIntegral n) (_targetDict s) }

addTokens :: (forall t. (InstructionSet t, Primitive (IM t), TargetPrimitive t) => IM t) -> FState a -> FState a
addTokens vs s = s { _targetDict = (_targetDict s) { _tdefining = f <$> _tdefining (_targetDict s) } }
  where f d = d { _tcompileList = _tcompileList d <> vs }

targetDictionary :: (InstructionSet t, Primitive (IM t), TargetPrimitive t) => TDict t
targetDictionary = TDict dict Nothing 0 
    where dict = fst $ newDictionary extras
          extras = do
            addWord "RSP0" resetRStack      -- reset return stack
            addWord "SP0"  resetStack       -- reset data stack

-- | Dummy binding a target dictionary is a kludge that can be used in certain situations
--   when we do not care which target it is, but the type system insists that it must know,
--   even though it does not matter.
--   Maybe there is a better way to do this, but I yet to find it.
arbitraryTargetDict :: FState a -> TDict ARMInstr
arbitraryTargetDict = _targetDict
