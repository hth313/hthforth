{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Block handling.

-}

module Forth.Block (readBlockFile) where

import Data.Char
import Data.List
import System.IO
import Numeric
import Data.Map (Map)
import qualified Data.Map as Map

data Kind = BlockKind | ShadowKind deriving Eq
data Block = Block { number :: Int, kind :: Kind, text :: String }

-- | Blocks are read from a file assumed to be editied using Emacs forthblocks mode.
--   An entire file is read and all blocks are read out from it and delivered as
--   two maps, one for the blocks and one for the shadow blocks.
readBlockFile :: FilePath -> IO (Map Int String, Map Int String)
readBlockFile filepath =
    let shadow line = isPrefixOf shadowPrefix line
        shadowPrefix = "( shadow "
        block line =  isPrefixOf blockPrefix line
        blockPrefix = "( block "
        header line = block line || shadow line
        blocksplit [] = []
        blocksplit (x:xs)
            | block x = Block n BlockKind (unlines lines) : blocksplit rest
            | shadow x = Block n ShadowKind (unlines lines) : blocksplit rest
            | otherwise = blocksplit xs
            where
              (n, x') =
                  let numstr = dropWhile isSpace (snd (break isSpace (drop 6 x)))
                  in case readDec numstr of
                       [(n,rest)] -> (n, rest)
              lines = x : lines'
              (lines', rest) = break header xs
        blockMap blocks =
            Map.fromList (map (\block -> (number block, text block)) blocks)
    in do
      contents <- readFile filepath
      let (blocks, shadows) = partition ((BlockKind==).kind)
                              (blocksplit (lines contents))
      return (blockMap blocks, blockMap shadows)
