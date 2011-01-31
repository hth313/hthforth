{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth core words in native lambdas.

-}

module Forth.Core () where

import Data.Bits
import Forth.Configuration
import Forth.DataField
import Forth.Machine
import Control.Monad.State.Lazy
import qualified Data.Map as Map

binary :: (ForthValue cell -> ForthValue cell -> ForthValue cell) ->
          StateT (Machine cell) IO ()
binary op = do
  ensureStack 2
  update (\s ->
      let tos : nos : rest = stack s
      in s { stack = op tos nos : rest })

unary op = do
  ensureStack 1
  update (\s ->
      let tos : rest = stack s
      in s { stack = op tos : rest })

updateStack n f = do
  ensureStack n
  update (\s -> s { stack = f (stack s) })

instance (Eq cell, Show cell, Num cell, Integral cell) => Num (ForthValue cell) where
    (Val a) + (Val b) = Val (a + b)
    (Address key off) + (Val b) = Address key (off + (fromIntegral b))
    (Val a) * (Val b) = Val (a * b)
    abs (Val a) = Val (abs a)
    signum (Val a) = Val (signum a)
    fromInteger n = Val (fromInteger n)

instance (Eq cell, Show cell, Bits cell, Integral cell) => Bits (ForthValue cell) where
    (Val a) .&. (Val b) = Val (a .&. b)
    (Val a) .|. (Val b) = Val (a .|. b)
    xor (Val a) (Val b) = Val (xor a b)
    complement (Val a) = Val (complement a)
    bitSize (Val a) = bitSize a
    isSigned (Val a) = isSigned a

-- TODO: validate stack size
ensureStack, ensureReturnStack :: Int -> ForthLambda cell
ensureStack n = return ()
ensureReturnStack n = return ()

words = mapM_ native [-- Data stack
                      ("DROP", updateStack 1 tail),
                      ("DUP", updateStack 1 (\st -> head st : st)),
                      ("OVER", updateStack 2 (\st -> head (tail st) : st)),
                      ("SWAP", updateStack 2 (\(s1:s2:ss) -> s2:s1:ss)),
                      ("ROT", updateStack 3 (\(s1:s2:s3:ss) -> s3:s1:s2:ss)),
                      -- Return stack
                      (">R", tor),
                      (">R", rto),
                      ("R@", rfetch),
                      -- ALU
                      ("+", binary (+)),
                      ("*", binary (+)),
                      ("-", binary (-)),
                      ("AND", binary (.&.)),
                      ("OR", binary (.|.)),
                      ("XOR", binary xor),
                      -- Load and store
                      ("!", store Cell),
                      ("C!", store (Byte . fromIntegral)),
                      ("@", fetch cellValue),
                      ("C@", fetch charValue),
                      -- Cell size and address related
                      ("CHAR+", unary (1+)), -- characters are just bytes
                      ("CHARS", unary id),
                      ("CELL+", cellSize >>= \n -> unary (Val n +)),
                      ("CELLS", cellSize >>= \n -> unary (Val n *))]
    where native (name, fun) =
              addWord $ ForthWord name False (Code (Just fun) Nothing Nothing)

tor = do
  ensureStack 1
  update (\s -> s { rstack = head (stack s) : rstack s, stack = tail (stack s) })

rto = do
  ensureReturnStack 1
  update (\s -> s { stack = head (rstack s) : stack s, rstack = tail (rstack s) })

rfetch = do
  ensureReturnStack 1
  update (\s -> s { stack = head (rstack s) : stack s })

store ctor = do
  ensureStack 2
  update (\s ->
      case stack s of
        (Val tos) : adr@(Address key offsetadr) : rest ->
            let (word, offset, field) = addressField adr s
                field' = storeData (ctor tos) offset field
                write _ = Just $ word { body = Data field' }
            in s { stack = rest,
                   wordKeyMap = Map.update write key (wordKeyMap s) }
         )

addressField (Address key offset) s =
    case Map.lookup key (wordKeyMap s) of
      Just word -> case body word of
                     Data field -> (word, offset, field)

fetch fval = do
  ensureStack 1
  update (\s -> case stack s of
                  adr@(Address key offsetadr) : rest ->
                      let (word, offset, field) = addressField adr  s
                          val = fval $ fetchData offset field
                      in s { stack = val : rest } )

cellValue (Cell val) = Val val
cellValue (Byte val) = Val 0 -- TODO: error
charValue (Byte val) = Val (fromIntegral val)
charValue (Cell val) = Val 0 -- TODO: error