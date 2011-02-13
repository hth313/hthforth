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
import Forth.DataField hiding (conf)
import Forth.Machine
--import Forth.Input
import qualified Data.Map as Map
import Text.Parsec.Error
import System.IO

binary :: Cell cell => (ForthValue cell -> ForthValue cell -> ForthValue cell) -> String ->
          ForthLambda cell
binary op name = ensureStack name [isValue, isValue] action where
    action = update (\s ->
                 let tos : nos : rest = stack s
                 in s { stack = op tos nos : rest })

unary :: Cell cell => (ForthValue cell -> ForthValue cell) -> String -> ForthLambda cell
unary op name = ensureStack name [isValue] action where
    action = update (\s ->
                 let tos : rest = stack s
                 in s { stack = op tos : rest })

updateStack n f name = ensureStack name (replicate n isAny) action where
    action = update (\s -> s { stack = f (stack s) })

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

ensureStack, ensureReturnStack ::
    Cell cell => String -> [ForthValue cell -> Bool] -> ForthLambda cell -> ForthLambda cell
ensureStack = ensure stack
ensureReturnStack = ensure rstack

ensure :: Cell cell => (Machine cell -> ForthValues cell) -> String ->
          [ForthValue cell -> Bool] -> ForthLambda cell -> ForthLambda cell
ensure stack name preds action = do
  s <- readMachine stack
  if length preds > length s
      then liftIO $ hPutStrLn stderr ("Empty stack for " ++ name)
      else
          let pairs = (zip preds s)
              vals = map (\(f,a) -> f a) pairs
          in if and vals
                then action
                else liftIO $ hPutStrLn stderr ("Bad stack argument for " ++ name ++
                                                ", stack is " ++ show (map snd pairs))

isValue (Val _) = True
isValue _ = False
isAddress (Address _ _) = True
isAddress _ = False
isAny = const True

-- | Define native and word header related words as lambdas
nativeWords :: Cell cell => ForthLambda cell
nativeWords =
  sequence_ (map native [-- Data stack
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
                         ("*", binary (*)),
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
                         ("CELL+", (\name -> cellSize >>= \n -> unary (Val n +) name)),
                         ("CELLS", (\name -> cellSize >>= \n -> unary (Val n *) name)),
                         -- Lambda versions of compiler words
                         ("IMMEDIATE", immediateWord),
                         --                      ("CREATE", create),
                         --                      ("POSTPONE", postpone)
                         ("EXIT", exit),
                         -- Block related
                         ("(LOAD)", loadScreen)
                     ] ++ map variable [ "BLK" ])
    where native (name, fun) =
              addWord $ ForthWord name False (Just (Code (Just (fun name)) Nothing Nothing))
          variable name = do
            sz <- cellSize
            conf <- readMachine conf
            addWord $ ForthWord name False (Just (Data (allot sz conf)))

tor :: Cell cell => String -> ForthLambda cell
tor name = ensureStack name [isAny] action where
    action = update (\s -> s { rstack = head (stack s) : rstack s, stack = tail (stack s) })

rto :: Cell cell => String -> ForthLambda cell
rto name = ensureReturnStack name [isAny] action where
    action = update (\s -> s { stack = head (rstack s) : stack s, rstack = tail (rstack s) })

rfetch :: Cell cell => String -> ForthLambda cell
rfetch name = ensureReturnStack name [isAny] action where
    action = update (\s -> s { stack = head (rstack s) : stack s })

store :: Cell cell => (cell -> DataObject cell) -> String -> StateT (Machine cell) IO ()
store ctor name = ensureStack name [isAddress, isValue] action where
    action =
        update (\s ->
            case stack s of
              adr@(Address key offsetadr) : (Val tos) : rest ->
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

fetch :: Cell cell =>
         (DataObject cell -> ForthValue cell) -> String -> StateT (Machine cell) IO ()
fetch fval name = ensureStack name [isAddress] action where
  action = update (\s -> case stack s of
               adr@(Address key offsetadr) : rest ->
                   let (word, offset, field) = addressField adr  s
                       val = fval $ fetchData offset field
                   in s { stack = val : rest }
               otherwise -> s  -- TODO: could use an error here
         )


cellValue (Cell val) = Val val
cellValue (Byte val) = Val 0 -- TODO: error
cellValue Undefined = UndefinedValue
charValue (Byte val) = Val (fromIntegral val)
charValue (Cell val) = Val 0 -- TODO: error
charValue Undefined = UndefinedValue

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

immediateWord :: Cell cell => String -> ForthLambda cell
immediateWord name =
  update (\s ->
      case lastWord s of
        Just key -> s { wordKeyMap = Map.update (\word -> Just $ word { immediate = True })
                                     key (wordKeyMap s) }
        Nothing -> s)

exit :: Cell cell => String -> ForthLambda cell
exit name  =
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

loadScreen :: Cell cell => String -> ForthLambda cell
loadScreen name = ensureStack name [isValue] action where
    action = do
      n <- StateT (\s ->
               case stack s of
                 (Val n) : ns -> return (n, s { stack = ns }))
      result <- load (fromIntegral n)
      case result of
        Left err -> liftIO $ hPutStrLn stderr err
        Right () -> return ()
