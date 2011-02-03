{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Block handling.

-}

module Forth.Block (readBlockFile) where

import Data.Char
import Data.List
import System.IO
import qualified Data.ByteString.Char8 as B
import Numeric
import Data.Map (Map)
import qualified Data.Map as Map

data Kind = BlockKind | ShadowKind deriving Eq
data Block = Block { number :: Int, kind :: Kind, blockLines ::  [B.ByteString] }

-- | Blocks are read from a file assumed to be editied using Emacs forthblocks mode.
--   An entire file is read and all blocks are read out from it and delivered as
--   two maps, one for the blocks and one for the shadow blocks.
readBlockFile :: FilePath -> IO (Map Int [B.ByteString], Map Int [B.ByteString])
readBlockFile filepath =
    let shadow line = B.isPrefixOf shadowPrefix line
        shadowPrefix = B.pack "( shadow "
        block line =  B.isPrefixOf blockPrefix line
        blockPrefix = B.pack "( block "
        header line = block line || shadow line
        blocksplit [] = []
        blocksplit (x:xs)
            | block x = Block n BlockKind lines : blocksplit rest
            | shadow x = Block n ShadowKind lines : blocksplit rest
            | otherwise = blocksplit xs
            where
              (n, x') =
                  let numstr = B.dropWhile isSpace (snd (B.break isSpace (B.drop 6 x)))
                  in case readDec (B.unpack numstr) of
                       [(n,rest)] -> (n, B.pack rest)
              lines = x' : lines'
              (lines', rest) = break header xs
        blockMap blocks =
            Map.fromList (map (\block -> (number block, blockLines block)) blocks)
    in do
      contents <- B.readFile filepath
      let (blocks, shadows) = partition ((BlockKind==).kind)
                              (blocksplit (B.split '\n' contents))
      return (blockMap blocks, blockMap shadows)
