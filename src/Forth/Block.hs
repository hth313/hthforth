{-# LANGUAGE OverloadedStrings #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Block handling.

-}

module Forth.Block (readBlockFile) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char
import Data.List
import System.IO
import Numeric
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

data Kind = BlockKind | ShadowKind deriving Eq
data Block = Block { number :: Int, kind :: Kind, text :: String }

-- | Blocks are read from a file assumed to be editied using Emacs forthblocks mode.
--   An entire file is read and all blocks are read out from it and delivered as
--   two maps, one for the blocks and one for the shadow blocks.
readBlockFile :: FilePath -> IO (IntMap ByteString, IntMap ByteString)
readBlockFile filepath =
    let shadow line = C.isPrefixOf shadowPrefix line
        shadowPrefix = "( shadow "
        block line =  C.isPrefixOf blockPrefix line
        blockPrefix = "( block "
        header line = block line || shadow line
        blocksplit [] = []
        blocksplit (x:xs)
            | block x = Block n BlockKind (C.unlines lines) : blocksplit rest
            | shadow x = Block n ShadowKind (C.unlines lines) : blocksplit rest
            | otherwise = blocksplit xs
            where
              (n, x') =
                  let numstr = C.dropWhile isSpace (snd (C.break isSpace (C.drop 6 x)))
                  in case readDec numstr of
                       [(n,rest)] -> (n, rest)
              lines = x : lines'
              (lines', rest) = C.break header xs
        blockMap blocks =
            IntMap.fromList (map (\block -> (number block, text block)) blocks)
    in do
      contents <- C.readFile filepath
      let (blocks, shadows) = partition ((BlockKind==).kind)
                              (blocksplit (C.lines contents))
      return (blockMap blocks, blockMap shadows)
