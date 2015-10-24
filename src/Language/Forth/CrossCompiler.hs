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
import qualified Data.IntMap as IntMap
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
import qualified Data.Vector.Storable.ByteString.Char8 as VC
import Translator.Expression
import Translator.Symbol

-- These imports are for binding a target type variable to a specific target,
-- which is sometimes needed. We pick Cortex-M in this case. In reality,
-- any target would do as we will not really be using the target in those
-- situations.
import Translator.Assembler.Target.ARM (ARMInstr)
import Language.Forth.Target.CortexM ()


-- | The cross compiler
-- crossCompiler :: (InstructionSet t, Primitive (IM t), TargetPrimitive t) => Compiler (IM t)
crossCompiler = Compiler defining compile litComma compileBranch compileBranch0 recurse startDefining
                         closeDefining abortDefining setImmediate reserveSpace where
  defining = isJust . _tdefining . arbitraryTargetDict
  compile (XT _ _ (Just tt)) = addTokens $ wordToken tt
  compile val@Val{} = litComma val
  litComma val = addTokens $ literal $ cellToExpr val
  compileBranch = compileBranch0
  compileBranch0 val s = s
  recurse s = s
  abortDefining s = s { _targetDict = abortDefining1 $ _targetDict s }
    where abortDefining1 dict = dict & tdefining.~Nothing
  setImmediate s = s { _targetDict = setImmediate1 $ _targetDict s }
  setImmediate1 dict = dict & tdict%~setLatestImmediate
  startDefining Create{..} s = s { _targetDict = startDefining1 $ _targetDict s }
    where startDefining1 dict = f $ dict { _tdefining = Just (TDefining createName doer) }
            where (f, doer) = case createStyle of
                                CREATE    -> (closeDefining1, dohere dict)
                                DOCOL     -> (id, docol)
                                DOCONST e -> (closeDefining1, doconst e)
  closeDefining s = s { _targetDict = closeDefining1 $ _targetDict s }
  closeDefining1 dict = dict & tdefining.~Nothing &
                               tdict.latest.~Just newWord &
                               twids.~twids' &
                               twords%~(IntMap.insert (unWordId wid) newWord) &
                               tlabels.~tlabels'
    where newWord = ForthWord name (Just sym) [] (_latest $ _tdict dict) wid Colon
                              (_tcompileList (fromJust $ _tdefining dict))
          (wid : twids') = dict^.twids
          name = _wordName (fromJust $ _tdefining dict)
          (sym, tlabels') = addEntityLabel wid (VC.unpack name) (dict^.tlabels)
  reserveSpace n s = s { _targetDict = targetAllot (fromIntegral n) (_targetDict s) }

addTokens :: (forall t. (InstructionSet t, Primitive (IM t), TargetPrimitive t) => IM t) -> FState a -> FState a
addTokens vs s = s { _targetDict = (_targetDict s) { _tdefining = f <$> _tdefining (_targetDict s) } }
  where f d = d { _tcompileList = _tcompileList d <> vs }

targetDictionary :: (InstructionSet t, Primitive (IM t), TargetPrimitive t) => TDict t
targetDictionary = TDict dict Nothing wids nativeWords labels
    where (dict, wids, Just labels) = newTargetDictionary extras (Just newLabels)
          nativeWords = IntMap.fromList $ (dict^.latest) & wordList
          wordList Nothing = []
          wordList (Just word) = (unWordId $ word^.wordId, word) : (word^.link & wordList)
          extras = do
            addWord "RSP0" Native resetRStack      -- reset return stack
            addWord "SP0"  Native resetStack       -- reset data stack

-- | Dummy binding a target dictionary is a kludge that can be used in certain situations
--   when we do not care which target it is, but the type system insists that it must know.
arbitraryTargetDict :: FState a -> TDict ARMInstr
arbitraryTargetDict = _targetDict
