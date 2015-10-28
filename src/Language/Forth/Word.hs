{-# LANGUAGE TemplateHaskell #-}{-|

  Forth word definition.

-}

module Language.Forth.Word (ForthWord(..), WordId(..), WordKind(..),
                            WordFlags(..), LinkField, wordFlags, hasFlag,
                            name, wordSymbol, link, doer, wordId, wordKind,
                            maxNameLen,
                            exitName, pdoName, ploopName, pploopName,
                            pleaveName) where

import Control.Lens
import Data.Char
import Translator.Symbol

-- | Unique identifier for words.
newtype WordId = WordId { unWordId :: Int } deriving (Eq, Ord)
instance Show WordId where
  show (WordId n) = show n

type LinkField a = Maybe (ForthWord a)

data WordKind = Native | Colon | InterpreterNative deriving Eq

data WordFlags = Immediate | CompileOnly deriving Eq

-- | A Forth word
data ForthWord a = ForthWord
  { _name :: String
  , _wordSymbol :: Maybe Symbol  -- ^ Symbol used, valid for target words
  , _wordFlags :: [WordFlags]
  , _link :: LinkField a
  , _wordId :: WordId
  , _wordKind :: WordKind
  , _doer :: a
  }
makeLenses ''ForthWord

instance Eq (ForthWord a) where
    a == b = _wordId a == _wordId b

instance Show (ForthWord a) where
    show = _name

-- | Limit words to this length
maxNameLen :: Int
maxNameLen = 31

-- | Test if a word has a certain flag set
hasFlag flag word = word^.wordFlags & elem flag

exitName, pdoName, ploopName, pploopName, pleaveName :: String
exitName   = "EXIT"
pdoName    = "(DO)"
ploopName  = "(LOOP)"
pploopName = "(+LOOP)"
pleaveName = "(LEAVE)"
