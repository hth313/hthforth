{-# LANGUAGE TemplateHaskell #-}{-|

  Forth word definition.

-}

module Language.Forth.Word (ForthWord(..), WordId(..), LinkField, immediateFlag,
                            name, link, doer, wordId, nameSymbol) where

import Control.Lens
import Data.Char
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as B
import qualified Data.ByteString.Char8 as L

-- | Unique identifier for words.
newtype WordId = WordId { unWordId :: Int } deriving (Eq, Ord)
instance Show WordId where
  show (WordId n) = show n

type LinkField a = Maybe (ForthWord a)

-- | A Forth word
data ForthWord a = ForthWord
  { _name :: ByteString
  , _immediateFlag :: Bool
  , _link :: LinkField a
  , _wordId :: WordId
  , _doer :: a
  }
makeLenses ''ForthWord

instance Eq (ForthWord a) where
    a == b = _wordId a == _wordId b

instance Show (ForthWord a) where
    show = B.unpack . _name

nameSymbol = L.pack . B.unpack . _name
