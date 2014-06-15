module Language.Forth.WordId (WordId(..)) where

-- | Unique identifier for words.
newtype WordId = WordId Int deriving (Eq, Ord)
instance Show WordId where
  show (WordId n) = show n
