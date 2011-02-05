{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth core words in native lambdas.

-}

module Forth.Core () where

import Control.Monad
import Data.Bits
import Forth.Configuration
import Forth.DataField
import Forth.Machine
import Forth.Input
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
                      ("CELLS", cellSize >>= \n -> unary (Val n *)),
                      -- Lambda versions of compiler words
                      ("IMMEDIATE", immediateWord),
                      ("CREATE", create),
                      ("EXIT", exit),
                      ("POSTPONE", postpone)

        ]
    where native (name, fun) =
              addWord $ ForthWord name False (Just (Code (Just fun) Nothing Nothing))

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
                write _ = Just $ word { body = Just (Data field') }
            in s { stack = rest,
                   wordKeyMap = Map.update write key (wordKeyMap s) }
         )

addressField (Address key offset) s =
    case Map.lookup key (wordKeyMap s) of
      Just word -> case body word of
                     Just (Data field) -> (word, offset, field)

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

create = do
  name <- nextWord
  update (\s ->
      let newWord = ForthWord name False Nothing key
          (key : keys') = keys s
      in s { keys = keys', lastWord = Just key,
             wordKeyMap = Map.insert key newWord (wordKeyMap s),
             wordNameMap = Map.insert name key (wordNameMap s) })

immediateWord =
  update (\s ->
      case lastWord s of
        Just key -> s { wordKeyMap = Map.update (\word -> Just $ word { immediate = True })
                                     key (wordKeyMap s) }
        Nothing -> s)

exit =
  update (\s ->
      let (Continuation slice : rstack') = rstack s
      in s { rstack = rstack', ip = slice })

postpone = do
  word <- nextWord
  compile word

compile word =
  update (\s ->
      case Map.lookup word (wordNameMap s) of
        Just wordkey ->
            let elt = [WordRef wordkey]
                f def =
                    let body' = case body def of
                                  Nothing -> Code Nothing Nothing (Just elt)
                                  Just body ->
                                      body { colon = case colon body of
                                                       Nothing -> Just elt
                                                       Just colon -> Just (colon ++ elt) }
                    in Just $ def { body = Just body' }
            in case lastWord s of
                 Just key -> s { wordKeyMap = Map.update f key (wordKeyMap s) })

-- | TODO: this one needs to be a colon definition, need to use BEGIN AGAIN to
--   avoid having stack build up when QUIT is invoked.
quit =
    let loop = do
          input <- liftIO getLine
          -- TODO: set >IN to 0
          -- TODO: write to input buffer
          update (\s -> s { stack = n : inputBuffer : stack s })
          evaluate
          putStrLn " ok "
    in do
      update (\s -> s { rstack = [] })
      -- TODO: enter interpretation state
      forever loop

evaluate = do
  -- TODO: push input buffer state
