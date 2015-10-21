{-# LANGUAGE OverloadedStrings, RankNTypes, FlexibleContexts, TemplateHaskell #-}
{- |

   Build the dictionary.

-}

module Language.Forth.Dictionary (newInterpreterDictionary, newTargetDictionary,
                                  IDict(..), TDict(..), Dictionary(..),
                                  idict, idefining, compileList,
                                  latest, tdict, tdefining, twids, twords,
                                  tcompileList, wordName,
                                  DefiningWrapper(..), TDefining(..), hereRAM,
                                  IDefining(..),
                                  definingWord, patchList,
                                  stateWId, toInWId,
                                  inputBufferWId, inputLineWId, tregWid,
                                  inputLineLengthWId, wordBufferWId, sourceIDWid,
                                  addWord, makeImmediate, setLatestImmediate,
                                  targetAllot) where

import Control.Lens hiding (over)
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State hiding (state)
import Data.IntMap (IntMap)
import Data.Vector.Storable.ByteString.Char8 (ByteString)
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
import Prelude hiding (drop, or, and)

data IDict a = IDict {
    _idict  :: Dictionary a,
    _idefining :: Maybe (IDefining a)
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
}

data TDefining t = TDefining  {
    _wordName :: ByteString
  , _tcompileList :: IM t
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
  Primitive a => State (Dictionary a, [WordId]) () -> (Dictionary a, [WordId])
newInterpreterDictionary = newDictionary wordsIds
newTargetDictionary = newDictionary allIds

-- Create a new basic dictionary.
newDictionary :: Primitive a => [WordId] -> State (Dictionary a, [WordId]) () -> (Dictionary a, [WordId])
newDictionary wids extras = execState build (Dictionary Nothing (Value 0), wids)
  where
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
  StateT $ \(Dictionary latest hereRAM, i:is) ->
      return ((), (Dictionary (Just $ ForthWord name False latest i kind doer) hereRAM, is))

makeImmediate :: State (Dictionary a, [WordId]) ()
makeImmediate = modify (_1%~setLatestImmediate)

setLatestImmediate = latest._Just.immediateFlag.~True

-- | Reserve space in data memory
targetAllot :: Expr -> TDict t -> TDict t
targetAllot n = tdict.hereRAM%~(+n)
