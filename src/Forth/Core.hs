{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth core words in native lambdas.

-}

module Forth.Core (nativeWords) where

import Control.Monad
import Data.Bits
import Forth.Cell
import Forth.Configuration
import Forth.DataField
import Forth.Machine
--import Forth.Input
import qualified Data.Map as Map
import Text.Parsec.Error
import System.IO

binary :: Cell cell => (ForthValue cell -> ForthValue cell -> ForthValue cell) ->
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

instance Cell cell => Num (ForthValue cell) where
    (Val a) + (Val b) = Val (a + b)
    (Address key off) + (Val b) = Address key (off + (fromIntegral b))
    (Val a) * (Val b) = Val (a * b)
    abs (Val a) = Val (abs a)
    signum (Val a) = Val (signum a)
    fromInteger n = Val (fromInteger n)

instance Cell cell => Bits (ForthValue cell) where
    (Val a) .&. (Val b) = Val (a .&. b)
    (Val a) .|. (Val b) = Val (a .|. b)
    xor (Val a) (Val b) = Val (xor a b)
    complement (Val a) = Val (complement a)
    bitSize (Val a) = bitSize a
    isSigned (Val a) = isSigned a

-- TODO: validate stack size
ensureStack, ensureReturnStack :: Cell cell => Int -> ForthLambda cell
ensureStack n = return ()
ensureReturnStack n = return ()

-- | Define native and word header related words as lambdas
nativeWords :: Cell cell => StateT (Machine cell) IO ()
nativeWords = mapM_ native [-- Data stack
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
                      ("0<", unary (\(Val n) -> if n < 0 then -1 else 0)),
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
--                      ("CREATE", create),
--                      ("POSTPONE", postpone)
                      ("EXIT", exit),
                      -- Block related
                      ("LOAD", loadScreen)

        ]
    where native (name, fun) =
              addWord $ ForthWord name False (Just (Code (Just fun) Nothing Nothing))

tor :: Cell cell => ForthLambda cell
tor = do
  ensureStack 1
  update (\s -> s { rstack = head (stack s) : rstack s, stack = tail (stack s) })

rto :: Cell cell => ForthLambda cell
rto = do
  ensureReturnStack 1
  update (\s -> s { stack = head (rstack s) : stack s, rstack = tail (rstack s) })

rfetch :: Cell cell => ForthLambda cell
rfetch = do
  ensureReturnStack 1
  update (\s -> s { stack = head (rstack s) : stack s })

store :: Cell cell => (cell -> DataObject cell) -> StateT (Machine cell) IO ()
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

fetch :: Cell cell => (DataObject cell -> ForthValue cell) -> StateT (Machine cell) IO ()
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

{-
create = do
  name <- nextWord
  update (\s ->
      let newWord = ForthWord name False Nothing key
          (key : keys') = keys s
      in s { keys = keys', lastWord = Just key,
             wordKeyMap = Map.insert key newWord (wordKeyMap s),
             wordNameMap = Map.insert name key (wordNameMap s) })
-}

immediateWord :: Cell cell => ForthLambda cell
immediateWord =
  update (\s ->
      case lastWord s of
        Just key -> s { wordKeyMap = Map.update (\word -> Just $ word { immediate = True })
                                     key (wordKeyMap s) }
        Nothing -> s)

exit :: Cell cell => ForthLambda cell
exit =
  update (\s ->
      let (Continuation slice : rstack') = rstack s
      in s { rstack = rstack', ip = slice })

{-
postpone = do
  word <- nextWord
  compile word
-}

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

loadScreen :: Cell cell => ForthLambda cell
loadScreen = do
    n <- StateT (\s ->
             let (Val n) : ns = stack s
             in return (n, s { stack = ns }))
    result <- load (fromIntegral n)
    case result of
      Left err -> liftIO $ hPutStrLn stderr (show err)
      Right () -> return ()
