{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-

  Symbols.

-}

module Translator.Symbol (mkSymbol, addEntityLabel, module Data.Symbol,
                          newLabels, Labels(..), toLabel, fromLabel) where

import Data.Char
import Control.Lens
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Symbol


-- | Convert a string to a symbol
mkSymbol = intern

-- Assembler labels have a flat namespace, but this is not always
-- suitable when mapping entities from another language down.
-- This structure keeps track of existing labels and will provide
-- name mangling and postfixing a counter to it if needed to
-- ensure that a unique high level entity corresponds to one label.
data Labels e = Labels {
    _toLabel   :: Map e Symbol
  , _fromLabel :: Map Symbol e
  }

makeLenses ''Labels

-- | Given a suggested name associated with some entity, create
--   a unique label for the entity.
--   Returns the label and updated labels and entity mapping set.
addEntityLabel :: Ord e => e -> String -> Set Symbol -> Labels e -> (Symbol, Labels e)
addEntityLabel e name reserved labels =
  let label = makeUnique (nameMangle name) 0
      makeUnique name n | not (Map.member sym (labels^.fromLabel)),
                          not (Set.member sym reserved) = sym
                        | otherwise = makeUnique name (n + 1)
        where sym | n == 0 = mkSymbol name
                  | otherwise = mkSymbol $ name ++ "_" ++ show n
  in (label, labels { _toLabel = Map.insert e label (_toLabel labels),
                      _fromLabel = Map.insert label e (_fromLabel labels) }  )

newLabels = Labels Map.empty Map.empty

-- Ensure the name is something the assembler accepts.
nameMangle :: String -> String
nameMangle = prepend . concatMap mangle
  where mangle '@' = "_Fetch_"
        mangle '!' = "_Store_"
        mangle '+' = "_Plus_"
        mangle '-' = "_Minus_"
        mangle '*' = "_Star_"
        mangle '/' = "_Slash_"
        mangle '\'' = "_Tick_"
        mangle '=' = "_Equal_"
        mangle '>' = "_GreaterThan_"
        mangle '<' = "_LessThan_"
        mangle '"' = "_Quote_"
        mangle '\\' = "_BackSlash_"
        mangle c = [c]
        prepend s@(c:cs) | isDigit c = '_' : s
                         | otherwise = s
