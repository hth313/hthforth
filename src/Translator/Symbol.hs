{-

  Symbols.

-}

module Translator.Symbol (Symbol, nameMangle) where

import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Char

type Symbol = ByteString

  -- Ensure the name is something the assembler accepts.
nameMangle :: Symbol -> Symbol
nameMangle = pack . prepend . concatMap mangle . unpack
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
