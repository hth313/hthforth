{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth core words in native lambdas.

-}

module Forth.Core (nativeWords) where

import Data.Word
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Forth.Cell
import Forth.DataField
import Forth.Machine
import Forth.Types
--import Forth.Input
import qualified Data.Map as Map
import Text.Parsec.Error
import System.IO

binary :: Cell cell => (ForthValue cell -> ForthValue cell -> ForthValue cell) -> String ->
          ForthLambda cell
binary op name = ensureStack name [isAny, isAny] action where
    action = update (\s ->
                 let tos : nos : rest = stack s
                 in s { stack = op tos nos : rest })

unary :: Cell cell => (ForthValue cell -> ForthValue cell) -> String -> ForthLambda cell
unary op name = ensureStack name [isAny] action where
    action = update (\s ->
                 let tos : rest = stack s
                 in s { stack = op tos : rest })

updateStack :: Cell cell => Int -> (ForthValues cell -> ForthValues cell) -> String ->
               (ForthLambda cell)
updateStack n f name = ensureStack name (replicate n isAny) action where
    action = update (\s -> s { stack = f (stack s) })

instance Cell cell => Num (ForthValue cell) where
    (Val a) + (Val b) = Val (a + b)
    (Address key off) + (Val b) = Address key (off + (fromIntegral b))
    (Val b) + (Address key off) = Address key (off + (fromIntegral b))
    a + b = Bottom $ show a ++ " " ++ show b ++ " +"
    (Val a) - (Val b) = Val (a - b)
    (Address key off) - (Val b) = Address key (off - (fromIntegral b))
    a - b = Bottom $ show a ++ " " ++ show b ++ " -"
    (Val a) * (Val b) = Val (a * b)
    a * b = Bottom $ show a ++ " " ++ show b ++ " *"
    abs (Val a) = Val (abs a)
    abs a = Bottom $ show a ++ " ABS"
    negate (Val a) = Val (negate a)
    negate a = Bottom $ show a ++ " NEGATE"
    signum (Val a) = Val (signum a)
    signum a = Bottom $ show a ++ " SIGNUM"
    fromInteger n = Val (fromInteger n)

instance Cell cell => Bits (ForthValue cell) where
    (Val a) .&. (Val b) = Val (a .&. b)
    a .&. b = Bottom $ show a ++ " " ++ show b ++ " AND"
    (Val a) .|. (Val b) = Val (a .|. b)
    a .|. b = Bottom $ show a ++ " " ++ show b ++ " OR"
    xor (Val a) (Val b) = Val (xor a b)
    xor a b = Bottom $ show a ++ " " ++ show b ++ " XOR"
    complement (Val a) = Val (complement a)
    complement a = Bottom $ show a ++ " INVERT"
    bitSize (Val a) = bitSize a
    isSigned (Val a) = isSigned a
    isSigned _ = False

instance Cell cell => Ord (ForthValue cell) where
    compare (Val a) (Val b) = compare a b

true, false :: Cell cell => ForthValue cell
true = Val (-1)
false = Val 0

-- | Define native and word header related words as lambdas
nativeWords :: Cell cell => MachineM cell ()
nativeWords = do
  -- Data stack
  native "DROP"  (updateStack 1 tail)
  native "DUP"   (updateStack 1 (\st -> head st : st))
  native "OVER"  (updateStack 2 (\st -> head (tail st) : st))
  native "SWAP"  (updateStack 2 (\(s1:s2:ss) -> s2:s1:ss))
  native "ROT"   (updateStack 3 (\(s1:s2:s3:ss) -> s3:s1:s2:ss))
  -- Return stack
  native ">R" tor
  native "R>" rto
  native "R@" rfetch
  -- ALU
  native "+" (binary (+))
  native "UM*" umstar
  native "UM/MOD" ummod
  native "M*" umstar
  native "-" (binary (-))
  native "AND" (binary (.&.))
  native "OR" (binary (.|.))
  native "XOR" (binary xor)
  native "0<" (unary (\n -> truth (n < 0)))
  native "0=" (unary (\n -> truth (n == 0)))
  native "U<" (binary ult)
  native "2/" (unary (`shiftR` 1))
  -- Load and store
  native "!" (store Cell)
  native "C!" (store (\(Val n) -> Byte (fromIntegral n)))
  native "@" (fetch cellValue)
  native "C@" (fetch charValue)
  -- Cell size and address related
  native "CHAR+" (unary (1+)) -- characters are just byt
  native "CHARS" (unary id)
  native "CELL+" (unitPlus cellSize)
  native "CELLS" (units cellSize)
  native "INSTR+" (unitPlus instructionSize)
  native "INSTRS" (units instructionSize)
  -- Lambda versions of compiler words
  native "IMMEDIATE" immediateWord
--                      native "CREATE" create
--                      native "POSTPONE" postpon
  native "EXIT" exit
-- DOES> part of CONSTANT and VARIABLE
  native "_VAR" doesVariable
  native "_CON" doesConstant
  -- Compiler related
--  native "LITERAL" literal
--  immediate
  native "_LIT" lit
  native ">BODY" toBody
  -- Block related
  native "(LOAD)" loadScreen
  native "THRU" thru
    where
      native :: Cell cell => String -> (String -> ForthLambda cell) -> MachineM cell ()
      native name fun = do
            key <- newKey
            addWord (ForthWord name False key Nothing (Just (fun name))
                               Nothing Nothing Nothing)
      unitPlus = unit (+)
      units = unit (*)
      unit op sz name = sz >>= \n -> unary (Val n `op`) name
      truth True = true
      truth False = false
--      immediate = immediateWord "IMMEDIATE"

tor :: Cell cell => String -> ForthLambda cell
tor name = ensureStack name [isAny] action where
    action = update (\s -> s { rstack = head (stack s) : rstack s, stack = tail (stack s) })

rto :: Cell cell => String -> ForthLambda cell
rto name = ensureReturnStack name [isAny] action where
    action = update (\s -> s { stack = head (rstack s) : stack s, rstack = tail (rstack s) })

rfetch :: Cell cell => String -> ForthLambda cell
rfetch name = ensureReturnStack name [isAny] action where
    action = update (\s -> s { stack = head (rstack s) : stack s })

store :: Cell cell => (ForthValue cell -> DataObject cell) -> String -> MachineM cell ()
store ctor name = ensureStack name [isAddress, isAny] action where
    action = do
      conf <- configuration
      update (\s ->
          case stack s of
            adr@(Address key offsetadr) : tos : rest ->
                let (word, offset, field) = addressField adr s
                    field' = storeData (ctor tos) offset field conf
                    write _ = Just $ word { dataField = Just field' }
                in s { stack = rest,
                       wordKeyMap = Map.update write key (wordKeyMap s) }
               )

addressField :: Cell cell => ForthValue cell -> Machine cell ->
                (ForthWord cell, cell, DataField cell)
addressField (Address key offset) s =
    case Map.lookup key (wordKeyMap s) of
      Just word -> case dataField word of
                     Just field -> (word, offset, field)

fetch :: Cell cell =>
         (DataObject cell -> ForthValue cell) -> String -> MachineM cell ()
fetch fval name = ensureStack name [isAddress] action where
  action = update (\s -> case stack s of
               adr@(Address key offsetadr) : rest ->
                   let (word, offset, field) = addressField adr s
                       val = fval $ fetchData offset field
                   in s { stack = val : rest }
               otherwise -> s  -- TODO: could use an error here
         )

cellValue :: Cell cell => DataObject cell -> ForthValue cell
cellValue (Cell (Val val)) = Val val
cellValue (Byte val) = UndefinedValue
cellValue Undefined = UndefinedValue

charValue :: Cell cell => DataObject cell -> ForthValue cell
charValue (Byte val) = Val (fromIntegral val)
charValue (Cell val) = UndefinedValue
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

{-
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
-}

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
thru :: Cell cell => String -> ForthLambda cell
thru name = ensureStack name [isValue, isValue] action where
    action = do
      range <- StateT (\s -> case stack s of
                               Val to : Val from : ns -> return ([from..to], s))
      word <- wordFromName "LOAD"
      case word of
        Just [load] -> do
            loadLit <- loadLiteralWordRef
            let makedef n = [loadLit, Literal (Val n), load]
            executeColonSlice (concatMap makedef range)

ult :: Cell cell => ForthValue cell -> ForthValue cell -> ForthValue cell
ult (Val n1) (Val n2) =
    let u1, u2 :: Word64
        u1 = fromIntegral n1
        u2 = fromIntegral n2
    in if u1 < u2 then true else false

-- Signed multiply to double cell
mstar :: Cell cell => String -> ForthLambda cell
mstar name = ensureStack name [isValue, isValue] action where
    action = update (\s -> case stack s of
                             Val n1 : Val n2 : stack' ->
                                 let v1, v2 :: Integer
                                     v1 = fromIntegral n1
                                     v2 = fromIntegral n2
                                     prod = v1 * v2
                                     lo = fromIntegral prod
                                     hi = fromIntegral $ prod `shiftR` (bitSize n1)
                                 in s { stack = Val hi : Val lo : stack' })

uext :: Cell cell => cell -> Integer
uext n = mask .&. (fromIntegral n) where
    mask = (1 `shiftL` bitSize n) - 1

-- Unsigned multiply to double cell
umstar :: Cell cell => String -> ForthLambda cell
umstar name = ensureStack name [isValue, isValue] action where
    action = update (\s -> case stack s of
                             Val n1 : Val n2 : stack' ->
                                 let v1 = uext n1
                                     v2 = uext n2
                                     prod = v1 * v2
                                     lo = fromIntegral prod
                                     hi = fromIntegral $ prod `shiftR` (bitSize n1)
                                 in s { stack = Val hi : Val lo : stack' })

-- Unsigned double cell to cell divide and remainder
ummod :: Cell cell => String -> ForthLambda cell
ummod name = ensureStack name [isValue, isValue, isValue] action where
    action = update (\s -> case stack s of
                             Val n3 : Val n2 : Val n1 : stack' ->
                                 let ud = uext n2 `shiftL` bitSize n1
                                     u = uext n3
                                     (quot, rem) = ud `quotRem` u
                                 in s { stack = Val (fromIntegral quot) :
                                                Val (fromIntegral rem) : stack'})

doesVariable :: Cell cell => String -> ForthLambda cell
doesVariable name = return ()

doesConstant :: Cell cell => String -> ForthLambda cell
doesConstant name = ensureStack name [isAddress] action where
    action = fetch cellValue name

-- | Words that need to exist, but do not have any defined behavior in the Haskell
--   emulated Forth as it is implemented in a different way. It will need a target
--   version though.
doNotCall :: Cell cell => String -> ForthLambda cell
doNotCall name =
    liftIO $ hPutStrLn stderr (show name ++ " not to be executed (intended for target)")

-- | Pick up next item in the colon definition which should be a literal and push
--   it on stack.
lit :: Cell cell => String -> ForthLambda cell
lit name = update (\s ->
               case ip s of
                 [] -> s  -- TODO why is this needed?
                 Literal val : ip' -> s { ip = ip', stack = val : stack s })

--    let Literal val : ip' = ip s
--    in s { ip = ip', stack = val : stack s })

toBody :: Cell cell => String -> ForthLambda cell
toBody name = ensureStack name [isExecutionToken] action where
    action = update (\s ->
                let ExecutionToken key : stack' = stack s
                in case Map.lookup key (wordKeyMap s) of
                     Just word ->
                         case dataField word of
                           Just field -> s { stack = Address key 0 : stack' })