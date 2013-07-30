module Forth.Word (ForthWord, name, IP) where

import Data.Vector.Storable.ByteString.Char8 (ByteString)

data ForthWord cell
instance Eq (ForthWord cell)
instance Show (ForthWord cell)

name :: ForthWord cell -> ByteString

data IP cell
instance (Eq cell) => Eq (IP cell)
instance (Show cell) => Show (IP cell)
