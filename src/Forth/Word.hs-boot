module Forth.Word (ForthWord, name, IP) where

data ForthWord cell
instance Eq (ForthWord cell)
instance Show (ForthWord cell)

name :: ForthWord cell -> String

data IP cell
instance Eq (IP cell)
instance Show (IP cell)
