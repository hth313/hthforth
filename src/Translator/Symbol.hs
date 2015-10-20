{-

  Symbols.

-}

module Translator.Symbol (mkSymbol, nameMangle, module Data.Symbol) where

import Data.Symbol
import Data.Char


-- | Convert a string to a symbol
mkSymbol = intern

  -- Ensure the name is something the assembler accepts.
nameMangle :: String -> Symbol
nameMangle = mkSymbol . prepend . concatMap mangle
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
