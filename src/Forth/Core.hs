{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth core words in native lambdas.

-}

module Forth.Core () where

import Forth.Configuration
import Forth.Machine
import Control.Monad.State.Lazy
import qualified Data.Map as Map

binary :: (ForthValue cell -> ForthValue cell -> ForthValue cell) ->
          StateT (Machine cell) IO ()
binary op = do
  ensureState2
  update (\s ->
      let tos : nos : rest = stack s
      in s { stack = op tos nos : rest })

instance (Eq cell, Show cell, Num cell, Integral cell) => Num (ForthValue cell) where
    (Val a) + (Val b) = Val (a + b)
    (Address key off) + (Val b) = Address key (off + (fromIntegral b))
    (Val a) * (Val b) = Val (a * b)
    abs (Val a) = Val (abs a)
    signum (Val a) = Val (signum a)
    fromInteger n = Val (fromInteger n)

-- TODO: validate stack size
ensureState1, ensureState2 :: ForthLambda cell
ensureState1 = return ()
ensureState2 = return ()

words = mapM_ native [("+", binary (+)),
                      ("*", binary (+)),
                      ("-", binary (-)),
                      ("!", store)]
    where native (name, fun) =
              addWord $ ForthWord name False (Code (Just fun) Nothing Nothing)


store = do
  ensureState2
  update (\s ->
      case stack s of
        (Val tos) : Address key offset : rest ->
            case Map.lookup key (wordKeyMap s) of
              Just word ->
                  case body word of
                    Data bytes
                      | length bytes >= offset + (bytesPerCell (conf s)) ->
                          s { stack = rest,
                              wordKeyMap = Map.update write key (wordKeyMap s) }
                        where write _ = Just $ word { body = Data bytes' }
                              bytes' = take offset bytes ++ toBytes (conf s) tos ++
                                       drop (4 + offset) bytes
         )
