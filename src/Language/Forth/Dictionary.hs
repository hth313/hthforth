{-# LANGUAGE OverloadedStrings #-}
{- |

   Build the dictionary.

-}

module Language.Forth.Dictionary (newDictionary, Dictionary(..)) where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Language.Forth.Primitive
import Language.Forth.Word
import Language.Forth.WordId

data Dictionary a = Dictionary
  { wids :: [WordId]
  , latest :: Maybe (ForthWord a)
  }

newDictionary :: Primitive c a => Dictionary a
newDictionary = execState build (Dictionary (map WordId [0..]) Nothing)
  where
    build = do
      addNative "(;)" semi
      addNative "SWAP" swap
      addNative "+" add

    addNative name doer = modify $ \s ->
                          let i:is = wids s
                          in  s { wids = is,
                                  latest = Just $ ForthWord name False (latest s) i doer }
