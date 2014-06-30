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
  evaluate, execute :: a
  lit :: c -> a
  false, true :: a
  swap, drop, dup, rot, over, rto, tor, rfetch :: a
  fetch, cfetch :: a
  store, plusStore :: a
  plus, minus, star, slash, and, or, xor :: a
  zerop, lt0 :: a
  quit :: a
  interpret :: a
  branch :: [a] -> a
  branch0 :: [a] -> a
  docol :: [a] -> a
  state :: a                  -- ^ STATE (compilation state)
  sourceId :: a               -- ^ SOURCE-ID (current input source)
  toIn :: a                   -- ^ >IN
  inputBuffer :: a            -- ^ Input buffer
  inputLine :: a              -- ^ INPUT-LINE
  inputLineLength :: a        -- ^ #INPUT-LINE
  sourceID :: a               -- ^ SOURCE-ID
  create, colon, semicolon, smudge, compileComma, immediate :: a
  constant :: a
  xif, xelse, xthen :: a
  xdo, loop, plusLoop, leave :: a
