{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}{-|

  Forth word definition.

-}

module Language.Forth.Word (ForthWord(..), WordId(..), WordKind(..),
                            LinkField, immediateFlag,
                            name, link, doer, wordId, wordKind,
                            symbol, nameSymbol,
                            exitName, pdoName, ploopName, pploopName,
                            targetColonWordId) where

import Control.Lens
import Data.Char
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as L
import Translator.Symbol

-- | Unique identifier for words.
newtype WordId = WordId { unWordId :: Int } deriving (Eq, Ord)
instance Show WordId where
  show (WordId n) = show n

type LinkField a = Maybe (ForthWord a)

data WordKind = Native | Colon | InterpreterNative deriving Eq

-- | A Forth word
data ForthWord a = ForthWord
  { _name :: ByteString
  , _immediateFlag :: Bool
  , _link :: LinkField a
  , _wordId :: WordId
  , _wordKind :: WordKind
  , _doer :: a
  }
makeLenses ''ForthWord

instance Eq (ForthWord a) where
    a == b = _wordId a == _wordId b

instance Show (ForthWord a) where
    show = B.unpack . _name

symbol = mkSymbol . B.unpack
nameSymbol = symbol . _name

exitName, pdoName, ploopName, pploopName :: ByteString
exitName   = "EXIT"
pdoName    = "(DO)"
ploopName  = "(LOOP)"
pploopName = "(+LOOP)"

-- | Obtain an identity for a high level target word. At the moment we
--   just give them the same number what is different from native words
--   created newDictionary.
targetColonWordId = WordId 0
