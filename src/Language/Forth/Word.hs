{-|

  Forth word definition.

-}

module Language.Forth.Word (ForthWord(..), LinkField) where

import Data.Char
import Data.Vector.Storable.ByteString.Char8 (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as B
import Language.Forth.WordId


-- | A Forth word
data ForthWord a = ForthWord
  { name :: ByteString
  , immediate :: Bool
  , link :: LinkField a
  , wid :: WordId
  , doer :: a
  }

instance Eq (ForthWord a) where
    a == b = wid a == wid b

instance Show (ForthWord a) where
    show = B.unpack . name

type LinkField a = Maybe (ForthWord a)
