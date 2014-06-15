{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses #-}
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
class Primitive c a | a -> c where
  semi :: a
  execute :: a
  lit :: c -> a
  swap :: a
  add :: a
  quit :: a
  interpret :: a
  branch :: [a] -> a
  branch0 :: [a] -> a
  docol :: [a] -> a
