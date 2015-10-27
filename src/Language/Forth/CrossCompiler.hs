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

module Language.Forth.CrossCompiler (crossCompiler, targetDictionary,
                                     arbitraryTargetDict, targetColonHere) where

import Control.Applicative
import Control.Lens
import Data.Foldable (fold, foldMap)
import Data.Function
import Data.List
import qualified Data.Map as Map
import qualified Data.Vector as V
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
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
import Translator.Assembler.Generate (IM, labRec, insList, recWrap, sizeIM)
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
crossCompiler = Compiler defining compile litComma compileBranch compileBranch0
                         compileLoop compilePlusLoop
                         backpatch recurse startDefining
                         closeDefining abortDefining setImmediate reserveSpace where
  defining = isJust . _tdefining . arbitraryTargetDict
  compile (XT _ _ _ (Just tt)) = addTokens $ wordToken tt
  compile val@Val{} = litComma val
  compile (XT _ (Just name) _ _) = const (Left $ VC.unpack name ++ " ? (not known to target)")
  compile val = const (Left $ "cannot compile for target: " ++ show val)
  litComma val = addTokens $ literal $ cellToExpr val
  compileBranch   s = s { _targetDict = addBranch findBranch   0 (_targetDict s) }
  compileBranch0  s = s { _targetDict = addBranch findBranch0  0 (_targetDict s) }
  compileLoop     s = s { _targetDict = addBranch findLoop     0 (_targetDict s) }
  compilePlusLoop s = s { _targetDict = addBranch findPlusLoop 0 (_targetDict s) }
  backpatch (HereColon _ loc) (HereColon _ dest) s =
    s { _targetDict = bp $ _targetDict s }
      where bp dict = dict & tdefining._Just.tpatchList%~((:) (loc, dest)) &
                      tdefining._Just.tLocals%~(IntSet.insert dest)
  recurse s = s { _targetDict = addBranch findBranch0 1 $ _targetDict s }
  abortDefining s = s { _targetDict = abortDefining1 $ _targetDict s }
    where abortDefining1 dict = dict & tdefining.~Nothing
  setImmediate s = s { _targetDict = setImmediate1 $ _targetDict s }
  setImmediate1 dict = dict & tdict%~(addFlag Immediate)
  startDefining Create{..} s = s { _targetDict = startDefining1 $ _targetDict s }
    where startDefining1 dict =
            f $ dict & tdefining.~(Just (TDefining createName sym wid IntSet.empty doer [])) &
                       tlabels.~tlabels' &
                       twids.~wids'
              where (f, doer) = case createStyle of
                                  CREATE    -> (closeDefining1, dohere dict)
                                  DOCOL     -> (id, docol)
                                  DOCONST e -> (closeDefining1, doconst e)
                    (sym, tlabels') = addEntityLabel wid (VC.unpack createName) (dict^.tlabels)
                    (wid:wids') = dict^.twids
  closeDefining s = s { _targetDict = closeDefining1 $ _targetDict s }
  closeDefining1 dict = dict & tdefining.~Nothing &
                               tdict.latest.~Just newWord &
                               twords%~(IntMap.insert (unWordId wid) newWord)
    where newWord = ForthWord name (Just sym) [] (dict^.tdict.latest) wid Colon body'
          (Just (TDefining name sym wid locals body patchList)) = dict^.tdefining
          bps = map (_1%~(1+)) $ sortBy (compare `on` fst) patchList
          body' | IntSet.null locals = body
                | otherwise = f (zip [0..] (insList body)) (IntSet.toList locals) bps
                where f nxs@((n, x):nxs') nls@(nl:nls') bps
                        | nl == n = labRec (localSymbol sym n) <> f nxs nls' bps
                      f nxs@((n, x):nxs') nls bps@((loc, dest):bps')
                        | loc == n = labelOffset (localSymbol sym dest) <>
                                         f nxs' nls bps'
                      f nxs@((n, x):nxs') nls bps
                        | otherwise = recWrap x <> f nxs' nls bps
                      f nxs [] [] = mconcat $ map (recWrap . snd) nxs
  reserveSpace n s = s { _targetDict = targetAllot (fromIntegral n) (_targetDict s) }
  findBranch   dict = findTargetToken dict "BRANCH"
  findBranch0  dict = findTargetToken dict "BRANCH0"
  findLoop     dict = findTargetToken dict "(LOOP)"
  findPlusLoop dict = findTargetToken dict "(+LOOP)"
  addBranch fb dest dict =
    let Just defining = dict^.tdefining
        g cl = cl <> wordToken ttBranch <>
               labelOffset (localSymbol (defining^.tdefiningSymbol) dest)
        Just ttBranch = fb $ dict^.tdict
        addLocal | dest /= 0 = tdefining._Just.tLocals%~(IntSet.insert dest)
                 | otherwise = id
    in dict & tdefining._Just.tcompileList%~g & addLocal

addTokens :: (forall t. (InstructionSet t, Primitive (IM t), TargetPrimitive t) => IM t) -> FState a -> Either String (FState a)
addTokens vs s = Right $
   s { _targetDict = (_targetDict s) & tdefining._Just.tcompileList%~(<> vs) }


targetDictionary :: (InstructionSet t, Primitive (IM t), TargetPrimitive t) => TDict t
targetDictionary = TDict dict Nothing wids nativeWords labels
    where (dict, wids, Just labels) = newTargetDictionary extras (Just newLabels)
          nativeWords = IntMap.fromList $ (dict^.latest) & wordList
          wordList Nothing = []
          wordList (Just word) = (unWordId $ word^.wordId, word) : (word^.link & wordList)
          extras = do
            addWord "RSP0"     Native resetRStack      -- reset return stack
            addWord "SP0"      Native resetStack       -- reset data stack
            addWord "BRANCH"   Native branch
            addWord "BRANCH0"  Native branch0
            addWord ploopName  Native loop
            addWord pploopName Native plusLoop >> addFlagM OmitNext

-- | Dummy binding a target dictionary is a kludge that can be used in certain situations
--   when we do not care which target it is, but the type system insists that it must know.
arbitraryTargetDict :: FState a -> TDict ARMInstr
arbitraryTargetDict = _targetDict

-- | Get the current offset for the target word being defined. This is the here value
--   for a target colon definition.
targetColonHere :: FState a -> Maybe (CV a)
targetColonHere s = hereColon <$> ((arbitraryTargetDict s)^.tdefining)
  where hereColon tdef = HereColon (tdef^.twid) (tdef^.tcompileList & sizeIM)
