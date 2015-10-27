{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, TemplateHaskell #-}
{- |

   Build the dictionary.

-}

module Language.Forth.Dictionary (newInterpreterDictionary, newTargetDictionary,
                                  IDict(..), TDict(..), Dictionary(..),
                                  idict, iwids, idefining, compileList,
                                  latest, tdict, tdefining, twids, twords, tlabels,
                                  tcompileList, wordName, tdefiningSymbol, twid,
                                  tLocals, tpatchList,
                                  DefiningWrapper(..), TDefining(..), hereRAM,
                                  IDefining(..),
                                  definingWord, patchList,
                                  stateWId, toInWId,
                                  inputBufferWId, inputLineWId, tregWid,
                                  inputLineLengthWId, wordBufferWId, sourceIDWid,
                                  addWord, makeImmediate, addFlag, addFlagM,
                                  targetAllot,
                                  findTargetToken, findWord) where

import Control.Applicative
import Control.Lens hiding (over)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State hiding (state)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C
import Data.Vector (Vector)
import Language.Forth.Interpreter.Address
import Language.Forth.Interpreter.CellMemory
import Language.Forth.Interpreter.DataField
import Language.Forth.CellVal
import Language.Forth.Primitive
import Language.Forth.Target
import Language.Forth.Word
import Translator.Assembler.Generate (IM)
import Translator.Expression (Expr(..))
import Translator.Symbol
import Prelude hiding (drop, or, and)

data IDict a = IDict {
    _idict  :: Dictionary a
  , _iwids :: [WordId]
  , _idefining :: Maybe (IDefining a)
}

-- | The defining state for the interpreter.
--   We collect words into a Vector together with information about locations
--   to change when we have collected all.
data IDefining a = IDefining {
    _compileList :: Vector (DefiningWrapper a)
  , _patchList :: [(Int, Int)]               -- ^ (loc, dest) list to patch
  , _defineFinalizer :: [a] -> a
  , _definingWord :: ForthWord a
}

-- | Wrapper for words being compiled. This is used to keep track of branches
--   that are waiting to have their address fixed.
data DefiningWrapper a = WrapA a | WrapB ([a] -> a) | WrapRecurse

data TDict t = TDict {
    _tdict :: Dictionary (IM t)
  , _tdefining :: Maybe (TDefining t)
  , _twids :: [WordId]
  , _twords :: IntMap (ForthWord (IM t))
  , _tlabels :: Labels WordId
}

data TDefining t = TDefining  {
    _wordName :: ByteString
  , _tdefiningSymbol :: Symbol
  , _twid :: WordId
  , _tLocals :: IntSet         -- ^ Index for desired local labels
  , _tcompileList :: IM t
  , _tpatchList :: [(Int, Int)]               -- ^ (loc, dest) list to patch
}

data Dictionary a = Dictionary
  { _latest :: LinkField a
  , _hereRAM :: Expr         -- ^ datafield usage for targets
  }

makeLenses ''IDict
makeLenses ''IDefining
makeLenses ''TDict
makeLenses ''TDefining
makeLenses ''Dictionary

-- Word identities are used to identify a particular word in a unique way.
-- They are used to find mutable datafields, which are stored separately in
-- the Forth state of the interpreter.
-- Some words (typically variables) in the interpreter that are needed early
-- get their word identity preallocated here and we use the tail for the
-- rest of words.
allIds@(stateWId : toInWId : inputBufferWId : inputLineWId :
        inputLineLengthWId : wordBufferWId : sourceIDWid :
        tregWid : wordsIds) = map WordId [0..]

newInterpreterDictionary, newTargetDictionary ::
  Primitive a => State (Dictionary a, [WordId], Maybe (Labels WordId)) () ->
                 Maybe (Labels WordId) ->
                 (Dictionary a, [WordId], Maybe (Labels WordId))
newInterpreterDictionary = newDictionary wordsIds
newTargetDictionary = newDictionary allIds

-- Create a new basic dictionary.
newDictionary :: Primitive a =>
                 [WordId] ->
                 State (Dictionary a, [WordId], Maybe (Labels WordId)) () ->
                 Maybe (Labels WordId) ->
                (Dictionary a, [WordId], Maybe (Labels WordId))
newDictionary wids extras mLabels =
  execState build (Dictionary Nothing (Value 0), wids, mLabels)  where
    build = do
      addWord "EXIT"  Native exit
      addWord "EXECUTE" Native execute
      addWord "SWAP" Native swap
      addWord "DROP" Native drop
      addWord "OVER" Native over
      addWord "DUP"  Native dup
      addWord "R>"   Native rto
      addWord ">R"   Native tor
      addWord "R@"   Native rfetch
      addWord "+"    Native plus
      addWord "-"    Native minus
      addWord "AND"  Native and
      addWord "OR"   Native or
      addWord "XOR"  Native xor
      addWord "2*"   Native twoStar
      addWord "2/"   Native twoSlash
      addWord "LSHIFT" Native lshift
      addWord "RSHIFT" Native rshift
      addWord "0="   Native zerop
      addWord "0<"   Native lt0
      addWord "!"    Native store
      addWord "C!"   Native cstore
      addWord "@"    Native fetch
      addWord "C@"   Native cfetch
      addWord "CONSTANT" Native constant
      addWord "UM*" Native umstar
      addWord "UM/MOD" Native ummod
      extras

addWord name kind doer =
  StateT $ \(Dictionary latest hereRAM, i:is, mlabels) ->
    let (msym, mlabels') = case mlabels of
                             Nothing -> (Nothing, Nothing)
                             Just labels ->
                               let (sym, labels') = addEntityLabel i (C.unpack name) labels
                               in (Just sym, Just labels')
        word = ForthWord name msym [] latest i kind doer
    in return ((), (Dictionary (Just word) hereRAM, is, mlabels'))

makeImmediate :: State (Dictionary a, [WordId], Maybe (Labels WordId)) ()
makeImmediate = addFlagM Immediate

addFlagM flag = modify (_1.latest._Just.wordFlags%~(flag:))
addFlag flag = latest._Just.wordFlags%~(Immediate:)

-- | Reserve space in data memory
targetAllot :: Expr -> TDict t -> TDict t
targetAllot n = tdict.hereRAM%~(+n)

findTargetToken dict name = targetToken <$> findWord dict name  where
  targetToken word =
    let Just sym = word^.wordSymbol
    in TargetToken (word^.wordId) sym

findWord dict n = f (dict^.latest)
  where f jw@(Just word) | n == word^.name = jw
                         | otherwise = f (word^.link)
        f Nothing = Nothing
