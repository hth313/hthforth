{-# LANGUAGE MultiParamTypeClasses #-}
{- |

   Forth primitives.

   We use tagless final style here and later instanciate it with an interpreter
   or in the future with various compilation schemes.

-}

module Language.Forth.Primitive (Primitive(..)) where

{- | The Forth builtin primitives.

     Tagless final style relies on some type 'a' to fold on.
     The cell size is meant to be flexible, so we use a type
     variable. The actual tagless final fold type is used by
     cell values (execution token), so we have to parameterize
     CellVal with both type variables.
-}
class Primitive a where
  exit :: a
  execute :: a
  swap, drop, dup, over, rto, tor, rfetch :: a
  fetch, cfetch :: a
  store, cstore :: a
  plus, minus, and, or, xor :: a
  twoStar, twoSlash, lshift, rshift :: a
  zerop, lt0 :: a
  constant :: a
  umstar :: a                 -- ^ UM*
  ummod :: a                  -- ^ UM/MOD
