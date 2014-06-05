{-# LANGUAGE RoleAnnotations #-}
module Language.Forth.Word (ForthWord, name, IP) where

import Data.Vector.Storable.ByteString.Char8 (ByteString)

type role ForthWord nominal
data ForthWord cell
instance Eq (ForthWord cell)
instance Show (ForthWord cell)

name :: ForthWord cell -> ByteString

type role IP nominal
data IP cell
instance (Eq cell) => Eq (IP cell)
instance (Show cell) => Show (IP cell)
