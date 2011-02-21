{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth core words in native lambdas.

-}

module Forth.Core (nativeWords) where

import Data.Word
import Control.Monad
import Data.Bits
import Forth.Cell
import Forth.DataField
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

true, false :: Cell cell => ForthValue cell
true = -1
false = 0

-- | Define native and word header related words as lambdas
nativeWords :: Cell cell => ForthLambda cell
nativeWords =
  mapM_  native [-- Data stack
                 ("DROP", updateStack 1 tail),
                 ("DUP", updateStack 1 (\st -> head st : st)),
                 ("OVER", updateStack 2 (\st -> head (tail st) : st)),
                 ("SWAP", updateStack 2 (\(s1:s2:ss) -> s2:s1:ss)),
                 ("ROT", updateStack 3 (\(s1:s2:s3:ss) -> s3:s1:s2:ss)),
                 -- Return stack
                 (">R", tor),
                 ("R>", rto),
                 ("R@", rfetch),
                 -- ALU
                 ("+", binary (+)),
                 ("UM*", umstar),
                 ("UM/MOD", ummod),
                 ("M*", umstar),
                 ("-", binary (-)),
                 ("AND", binary (.&.)),
                 ("OR", binary (.|.)),
                 ("XOR", binary xor),
                 ("0<", unary (\(Val n) -> if n < 0 then true else false)),
                 ("0=", unary (\(Val n) -> if n == 0 then true else false)),
                 ("U<", binary ult),
                 ("2/", unary (`shiftR` 1)),
                 -- Load and store
                 ("!", store Cell),
                 ("C!", store (Byte . fromIntegral)),
                 ("@", fetch cellValue),
                 ("C@", fetch charValue),
                 -- Cell size and address related
                 ("CHAR+", unary (1+)), -- characters are just bytes
                 ("CHARS", unary id),
                 ("CELL+", unitPlus cellSize),
                 ("CELLS", units cellSize),
                 ("XT+", unitPlus executionTokenSize),
                 ("XTS", units executionTokenSize),
                 -- Lambda versions of compiler words
                 ("IMMEDIATE", immediateWord),
                 --                      ("CREATE", create),
                 --                      ("POSTPONE", postpone)
                 ("EXIT", exit),
                 -- Block related
                 ("(LOAD)", loadScreen),
                 ("THRU", thru)
             ]
    where native (name, fun) =
              addWord $ ForthWord name False (Just (Code (Just (fun name)) Nothing Nothing))
          unitPlus = unit (+)
          units = unit (*)
          unit op sz name = sz >>= \n -> unary (Val n `op`) name

tor :: Cell cell => String -> ForthLambda cell
tor name = ensureStack name [isAny] action where
    action = update (\s -> s { rstack = head (stack s) : rstack s, stack = tail (stack s) })

rto :: Cell cell => String -> ForthLambda cell
rto name = ensureReturnStack name [isAny] action where
    action = update (\s -> s { stack = head (rstack s) : stack s, rstack = tail (rstack s) })

rfetch :: Cell cell => String -> ForthLambda cell
rfetch name = ensureReturnStack name [isAny] action where
    action = update (\s -> s { stack = head (rstack s) : stack s })

store :: Cell cell => (cell -> DataObject cell) -> String -> (MachineM cell) ()
store ctor name = ensureStack name [isAddress, isValue] action where
    action = do
      conf <- configuration
      update (\s ->
          case stack s of
            adr@(Address key offsetadr) : (Val tos) : rest ->
                let (word, offset, field) = addressField adr s
                    field' = storeData (ctor tos) offset field conf
                    write _ = Just $ word { body = Just (Data field') }
                in s { stack = rest,
                       wordKeyMap = Map.update write key (wordKeyMap s) }
               )

addressField (Address key offset) s =
    case Map.lookup key (wordKeyMap s) of
      Just word -> case body word of
                     Just (Data field) -> (word, offset, field)

fetch :: Cell cell =>
         (DataObject cell -> ForthValue cell) -> String -> (MachineM cell) ()
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
      (n, rstack1, ip1) <-
          StateT (\s ->
              case stack s of
                (Val n) : ns -> return ((n, rstack s, ip s),
                                        s { stack = ns, ip = [], rstack = [] }))
      result <- load (fromIntegral n)
      update (\s -> s { ip = ip1, rstack = rstack1 })
      case result of
        Left err -> liftIO $ hPutStrLn stderr err
        Right () -> return ()

-- THRU, load a range of sceens. Implemented here since we do not have looping
-- capabilitty at Forth level from start.
thru name = ensureStack name [isValue, isValue] action where
    action = do
      range <- StateT (\s -> case stack s of
                               Val to : Val from : ns -> return ([from..to], s))
      word <- wordFromName "LOAD"
      case word of
        Just load ->
            let makedef n = [Literal (Val n), load]
            in executeColonSlice (concatMap makedef range)

ult :: Cell cell => ForthValue cell -> ForthValue cell -> ForthValue cell
ult (Val n1) (Val n2) =
    let u1, u2 :: Word64
        u1 = fromIntegral n1
        u2 = fromIntegral n2
    in if u1 < u2 then true else false

-- Signed multiply to double cell
mstar name = ensureStack name [isValue, isValue] action where
    action = update (\s -> case stack s of
                             Val n1 : Val n2 : stack' ->
                                 let v1, v2 :: Integer
                                     v1 = fromIntegral n1
                                     v2 = fromIntegral n2
                                     prod = v1 * v2
                                     lo = fromIntegral prod
                                     hi = fromIntegral $ prod `shiftR` (bitSize n1)
                                 in s { stack = hi : lo : stack' })

uext :: Cell cell => cell -> Integer
uext n = mask .&. (fromIntegral n) where
    mask = (1 `shiftL` bitSize n) - 1

-- Unsigned multiply to double cell
umstar name = ensureStack name [isValue, isValue] action where
    action = update (\s -> case stack s of
                             Val n1 : Val n2 : stack' ->
                                 let v1 = uext n1
                                     v2 = uext n2
                                     prod = v1 * v2
                                     lo = fromIntegral prod
                                     hi = fromIntegral $ prod `shiftR` (bitSize n1)
                                 in s { stack = hi : lo : stack' })

-- Unsigned double cell to cell divide and remainder
ummod name = ensureStack name [isValue, isValue, isValue] action where
    action = update (\s -> case stack s of
                             Val n3 : Val n2 : Val n1 : stack' ->
                                 let ud = uext n2 `shiftL` bitSize n1
                                     u = uext n3
                                     (quot, rem) = ud `quotRem` u
                                 in s { stack = fromIntegral quot : fromIntegral rem : stack'})