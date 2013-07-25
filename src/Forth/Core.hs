{-# LANGUAGE OverloadedStrings #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth core words in native lambdas.

-}

module Forth.Core (addNatives, quit) where

import Control.Monad.State.Lazy hiding (state)
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import Data.Char
import Data.Word
import Control.Monad
import Control.Monad.Trans
import Data.Bits
import Forth.Address
import Forth.Cell
import Forth.CellMemory
import Forth.DataField
import Forth.Machine
import Forth.Types
import Forth.Word
import Util.Memory
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import System.IO
import Numeric
import Prelude hiding (drop)


-- | Populate the vocabulary with a basic set of Haskell native words.
addNatives :: Cell cell => MachineM cell ()
addNatives = do
  addNative "+"     plus
  addNative "-"   $ binary (-)
  addNative "*"   $ binary (*)
  addNative "/"   $ binary divide
  addNative "AND" $ binary (.&.)
  addNative "OR"  $ binary (.|.)
  addNative "XOR" $ binary xor
  addNative "DUP"   dup
  addNative "ROT"   rot
  addNative "DROP"  drop
  addNative "OVER"  over
  addNative "SWAP"  swap
  addNative "!"     store
  addNative "+!"    plusStore
  addNative "@"     fetch
  addNative "C@"    cfetch
  addNative "FIND"  find
  addNative "-INTERPRET" interpret
  addVar    "-INPUT-BUFFER" inputBufferId Nothing
  addVar    "STATE" stateId (Just $ Val 0)
  addVar    "SOURCE-ID" sourceId (Just $ Val 0)
  addVar    ">IN" toInId  (Just $ Val 0)
      where
        divide (Val a) (Val b) = Val (a `div` b)
        divide a b = Bot $ show a ++ " / " ++ show b

plus, dup, drop, swap, over, rot, plusStore :: Cell cell => ForthLambda cell
plus = binary (+)

dup = modify $ \s ->
    case stack s of
      s0 : ss -> s { stack = s0 : s0 : ss }
      otherwise -> emptyStack

drop = modify $ \s ->
    case stack s of
      s0 : ss -> s { stack = ss }
      otherwise -> emptyStack

swap = modify $ \s ->
    case stack s of
      s0 : s1 : ss -> s { stack = s1 : s0 : ss }
      otherwise -> emptyStack

over = modify $ \s ->
    case stack s of
      s0 : s1 : ss -> s { stack = s1 : s0 : s1 : ss }
      otherwise -> emptyStack

rot = modify $ \s ->
    case stack s of
      s0 : s1 : s2 : ss -> s { stack = s2 : s0 : s1 : ss }
      otherwise -> emptyStack

plusStore = dup >> fetch >> rot >> plus >> swap >> store


-- | Perform a binary operation
binary op = modify $ \s ->
    case stack s of
      s0 : s1 : ss -> s { stack = s0 `op` s1 : ss  }
      otherwise -> emptyStack


inputBuffer, toIn, sourceID, state :: Cell cell => ForthLambda cell
inputBuffer = wordIdExecute inputBufferId
toIn = wordIdExecute toInId
sourceID = wordIdExecute sourceId
state = wordIdExecute stateId


fetch :: Cell cell => ForthLambda cell
fetch = modify $ \s ->
    case stack s of
      Address (Just adr@(Addr wid _)) : rest
          | Just (DataField cm) <- IntMap.lookup wid (variables s),
            Just x <- readCell adr cm ->
                s { stack = x : rest }

cfetch :: Cell cell => ForthLambda cell
cfetch = modify $ \s ->
    case stack s of
      Address (Just adr@(Addr wid i)) : rest
          | Just (BufferField bm) <- IntMap.lookup wid (variables s) ->
              let c = Val $ fromIntegral $ B.index (chunk bm) i
              in s { stack = c : rest }


store :: Cell cell => ForthLambda cell
store = modify $ \s ->
    case stack s of
      Address (Just adr@(Addr wid i)) : val : rest
          | Just (DataField cm) <- IntMap.lookup wid (variables s) ->
              s { variables = IntMap.insert wid (DataField $ writeCell val adr cm)
                              (variables s),
                  stack = rest }


-- | Find the name (counted string) in the dictionary
--   ( c-addr -- c-addr 0 | xt 1 | xt -1 )
find :: Cell cell => ForthLambda cell
find = do
  caddr <- pop
  findname <- countedText caddr
  modify $ \s ->
      let locate (Just word) | name word == findname = Just word
                             | otherwise = locate $ link word
          locate Nothing = Nothing
      in case locate (dictionaryHead s) of
           Just word
               | immediate word -> s { stack = Val 1 : XT word : (stack s) }
               | otherwise -> s { stack = Val (-1) : XT word : (stack s) }
           Nothing -> s { stack = Val 0 : caddr : stack s }


-- | Parse a name in the input stream. Returns address (within input stream)
--   to start of parsed word and length of word. Uses and updates >IN.
--   ( char "<spaces>ccc<space>" -- c-addr u )
parseName :: Cell cell => ForthLambda cell
parseName = do
  inputBuffer >> toIn >> fetch >> plus  -- build start address
  modify $ \s ->
    case stack s of
       Address (Just (Addr wid off)) : ss
          | Just (BufferField cmem) <- IntMap.lookup wid (variables s) ->
              let start = B.drop off (chunk cmem)
                  (skipCount, nameStart) = skipSpaces start
                  skipSpaces bs
                      | B.null bs = (0, bs)
                      | otherwise = skipSpaces1 0 bs where
                      skipSpaces1 n bs
                          | C.head bs == ' ' = skipSpaces1 (n + 1) (B.tail bs)
                          | otherwise = (n, bs)
                  name = C.takeWhile (' ' /=) nameStart
                  nameLength = B.length name
                  inAdjust = skipCount + nameLength
                  result = Address $ Just (Addr wid (skipCount + off))
              in s { stack = Val (fromIntegral inAdjust) :
                             Val (fromIntegral nameLength) : result : ss }
       otherwise -> abortWith "PARSE-NAME failed"
  toIn >> plusStore


-- | Copy word from given address with delimiter to a special transient area.
--   ( char "<chars>ccc<char>" -- c-addr )
xword :: Cell cell => ForthLambda cell
xword = do
  parseName
  modify $ \s ->
    case stack s of
      Val len : Address (Just (Addr wid off)) : ss
          | Just (BufferField cmem) <- IntMap.lookup wid (variables s) ->
              let nameLength = fromIntegral len
                  name = B.take nameLength $ B.drop off (chunk cmem)
                  counted = cmem { chunk = B.cons (fromIntegral nameLength) name }
                  result = Address (Just $ Addr wordBufferId 0)
              in s { stack = result : ss,
                     variables = IntMap.insert wordBufferId (BufferField counted) (variables s) }
      otherwise -> abortWith "WORD failed"


-- | Processes input text stored in the input buffer.
interpret :: Cell cell => ForthLambda cell
interpret = state >> fetch >> pop >>= interpretLoop where
    interpretLoop stateFlag = interpret1 where
        compiling = stateFlag /= Val 0
        interpret1 = do
          xword
          dup >> cfetch
          eol <- liftM (Val 0 ==) pop
          if eol then drop else do
              find
              val <- pop
              case val of
                Val 0 -> parseNumber >> interpret1       -- not found
                Val 1 -> execute >> interpret1           -- immediate word
                _ | compiling -> abortWith "compile word not implemented" -- normal word found
                  | otherwise -> execute >> interpret1

    parseNumber = parse =<< countedText =<< pop where
        parse bs = case readDec text of
                     [(x,"")] -> push $ Val x
                     otherwise -> abortWith $ text ++ " ?"
            where text = C.unpack bs


-- | Given a counted string, extract the actual text as an individual ByteString.
countedText (Address (Just (Addr wid off))) = gets $ \s ->
    case IntMap.lookup wid (variables s) of
      Just (BufferField cmem) ->
          let count = fromIntegral $ B.index (chunk cmem) off
          in B.take count $ B.drop (off + 1) (chunk cmem)
      otherwise ->
          case IntMap.lookup wid (wordMap s) of
            Just word -> abortWith $ "expected address pointing to char buffer for " ++
                           (C.unpack $ name word)
            otherwise -> abortWith "expected address pointing to char buffer"
countedText _ = abortWith "expected address"

quit :: Cell cell => ForthLambda cell
quit = do
  modify $ \s -> s { rstack = [] }
  push (Val 0) >> sourceID >> store
  mainLoop


mainLoop :: Cell cell => ForthLambda cell
mainLoop = do
  putField inputBufferId =<< liftM (textBuffer inputBufferId)
                           (liftIO $ B.hGetLine stdin)
  push (Val 0) >> toIn >> store
  interpret
  liftIO $ putStrLn "ok"
  mainLoop


-- | Define native and word header related words as lambdas
{-
nativeWords :: Cell cell => cell -> MachineM cell i ()
nativeWords cell = do
  -- Data stack
  native "DROP"  (updateStack 1 tail)
  native "DUP"   dup
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
  native "!" store
  native "C!" cstore
  native "@" (fetch cellValue)
  native "C@" (fetch charValue)
  -- Cell size and address related
  native "CHAR+" (unary (1+)) -- characters are just byt
  native "CHARS" (unary id)
  native "CELL+" (unitPlus $ bytesPerCell cell)
  native "CELLS" (units $ bytesPerCell cell)
  native "INSTR+" (unitPlus $ bytesPerInstruction cell)
  native "INSTRS" (units $ bytesPerInstruction cell)
  native "ABORT" abort
  native "QUIT" quit
  createVariable ">IN"
  -- Lambda versions of compiler words
  native "IMMEDIATE" immediateWord
--                      native "CREATE" create
--                      native "POSTPONE" postpon
--  native "EXIT" exit
-- DOES> part of CONSTANT and VARIABLE
  native "_VAR" doesVariable
--  native "_CON" doesConstant
  -- Compiler related
--  native "LITERAL" literal
--  immediate
--  native "_LIT" lit
--  native ">BODY" toBody
--  native "COMPILE," compileComma
--  native "," comma
  -- Block related
{-
  native "(LOAD)" loadScreen
  native "THRU" thru
-}
    where
      native name fun = add name (Just fun)

      createVariable name = do
        add name Nothing
        key <- gets $ head . dictionary
        modifyLastWord $ \word ->
            Just $ word { lambda = Just (pushVariableKey (wordKey word)) }
        modify $ \s ->
            s { variables = Map.insert key (DataField (bytesPerCell 0) True Map.empty)
                              (variables s) }

      add name mfun = addWord (Word name False mfun Nothing Nothing Nothing)

      unitPlus = unit (+)
      units = unit (*)
      unit op sz = unary (op $ Val sz)
      truth True = true
      truth False = false
--      immediate = immediateWord "IMMEDIATE"
-}

{-
dup = updateStack 1 $ \st -> head st : st

abort :: Cell cell => MachineM cell i ()
abort = do
  modify $ \s -> s { stack = [] }
  quit

quit :: Cell cell => MachineM cell i ()
quit = do
  modify $ \s -> s { rstack = [] }
  -- TODO: clear SOURCE-ID if present
  -- TODO: user input device is source
  -- TODO: interpretation state
  forever $ do
      cr
      text <- readInputLine
      modify $ \s -> s { inputBuffer = text }
      pushLiteral $ Val 0
      pushVariable ">IN"
      store
      interpret
      liftIO $ putStr " ok"

cr :: MachineM cell i ()
cr = liftIO $ putStrLn ""

pushVariable :: Cell cell => String -> MachineM cell i ()
pushVariable name = do
    Just key <- gets $ Map.lookup name . wordNameMap
    pushVariableKey key

pushVariableKey :: Cell cell => WordId -> MachineM cell i ()
pushVariableKey key = modify $ \s ->
    case Map.lookup key (variables s) of
      Just df -> s { stack = Address (BodyAdr key 0) : stack s }

pushLit lit = modify $ \s -> s { stack = lit : stack s }

source :: Cell cell => MachineM cell i ()
source = modify $ \s ->
    s { stack = Val (fromIntegral $ C.length $ inputBuffer s) :
                InputSource (inputBuffer s) : stack s }

interpret :: MachineM cell i ()
interpret = do
  pushVariable ">IN"
  dup
  fetch
  n <- gets (head . stack)
  buf <- gets inputBuffer
  let (word, rest) = break (' '==) $ C.dropWhile (' '==) $ C.drop n buf
  modify $ \s -> s { stack = Val (C.length buf - C.length rest) : tail (stack s) }
  store
  unless (C.empty word) $ do
      mres <- find word
      case mres of
        Just t@ExecutionToken{} -> do
            pushLit t
            execute
        Nothing -> putStrLn word
      interpret
  return ()

readInputLine :: Cell cell => MachineM cell i ByteString
readInputLine = liftIO C.getLine

tor :: Cell cell => MachineM cell i ()
tor = ensureStack [isAny] $ \s ->
    s { rstack = head (stack s) : rstack s, stack = tail (stack s) }

rto :: Cell cell => MachineM cell i ()
rto = ensureReturnStack [isAny] $ \s ->
    s { stack = head (rstack s) : stack s, rstack = tail (rstack s) }

rfetch :: Cell cell => MachineM cell i ()
rfetch = ensureReturnStack [isAny] $ \s ->
    s { stack = head (rstack s) : stack s }

store, cstore :: Cell cell => MachineM cell i ()
store = xstore Cell
cstore = xstore cstoreFun
cstoreFun (Val n) = Byte (fromIntegral n)

--store :: Cell cell => (ForthValue cell -> DataObject cell) -> String -> MachineM cell ()
xstore ctor = ensureStack [isBodyAddress, isAny] $ \s ->
    let adr : val : stack' = stack s
    in case adr of
         Address (BodyAdr key offsetadr) ->
             let (_, offset, field) = addressField adr s
                 field' = storeData (ctor val) offset field
              in s { variables = Map.insert key field' (variables s),
                     stack = stack' }
-}
{-
        ColonAddress key offset ->
            -- Will write to last location. This is OK for literals which we will
            -- see this way. For back patching control words, we rely on that we
            -- insert special words and ise compileStructure to fix branching.
            -- Thus, we can ignore the offset as it is, we only write at the end.
            compileWord (Literal val)
-}

--addressField :: Cell cell => ForthValue cell -> Machine cell ->
--                (ForthWord cell, cell, DataField cell)
{-
addressField (Address (Just (BodyAdr (WordId key) offset))) s =
    case IntMap.lookup key (variables s) of
      Just field -> (key, offset, field)
-}

--fetch :: Cell cell =>
--         (DataObject cell -> ForthValue cell) -> String -> MachineM cell ()
{-
fetch fval = ensureStack [isAddress] $ \s ->
    let adr : stack' = stack s
    in case adr of
         Address (Just (BodyAdr key offsetadr)) ->
             let (word, offset, field) = addressField adr s
                 val = fval $ fetchData offset field
             in s { stack = val : stack' }
         otherwise -> s  -- TODO: could use an error here
-}

-- cellValue :: Cell cell => DataObject cell -> ForthValue cell
{-
cellValue (Cell (Val val)) = Val val
cellValue (Byte val) = UndefinedValue
cellValue Undefined = UndefinedValue
-}

-- charValue :: Cell cell => DataObject cell -> ForthValue cell
{-
charValue (Byte val) = Val (fromIntegral val)
charValue (Cell val) = UndefinedValue
charValue Undefined = UndefinedValue
-}

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

{-
immediateWord :: Cell cell => ForthLambda cell i
immediateWord = modify $ \s ->
    case lastWord s of
      Just key -> s { wordKeyMap = Map.update (\word -> Just $ word { immediate = True })
                                   key (wordKeyMap s) }
      Nothing -> s
-}

-- exit :: Cell cell => String -> ForthLambda cell
{-
exit name  = modify $ \s ->
    let (Continuation slice : rstack') = rstack s
    in s { rstack = rstack', ip = slice }
-}

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

{-
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
-}

-- THRU, load a range of sceens. Implemented here since we do not have looping
-- capabilitty at Forth level from start.
{-
--thru :: Cell cell => String -> ForthLambda cell
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
-}

--ult :: Cell cell => ForthValue cell -> ForthValue cell -> ForthValue cell
{-
ult (Val n1) (Val n2) =
    let u1, u2 :: Word64
        u1 = fromIntegral n1
        u2 = fromIntegral n2
    in if u1 < u2 then true else false
-}

-- Signed multiply to double cell
{-
mstar :: Cell cell => ForthLambda cell
mstar = ensureStack [isValue, isValue] $ \s ->
    case stack s of
      Val n1 : Val n2 : stack' ->
          let v1, v2 :: Integer
              v1 = fromIntegral n1
              v2 = fromIntegral n2
              prod = v1 * v2
              lo = fromIntegral prod
              hi = fromIntegral $ prod `shiftR` (bitSize n1)
          in s { stack = Val hi : Val lo : stack' }
-}

{-
uext :: Cell cell => cell -> Integer
uext n = mask .&. (fromIntegral n) where
    mask = (1 `shiftL` bitSize n) - 1
-}

-- Unsigned multiply to double cell
{-
umstar :: (Bits cell, Cell cell, Num cell) => ForthLambda cell
umstar = ensureStack [isValue, isValue] $ \s ->
    case stack s of
      Val n1 : Val n2 : stack' ->
          let v1 = uext n1
              v2 = uext n2
              prod = v1 * v2
              lo = fromIntegral prod
              hi = fromIntegral $ prod `shiftR` (bitSize n1)
          in s { stack = Val hi : Val lo : stack' }
-}

-- Unsigned double cell to cell divide and remainder
{-
ummod :: Cell cell => ForthLambda cell
ummod = ensureStack [isValue, isValue, isValue] $ \s ->
    case stack s of
      Val n3 : Val n2 : Val n1 : stack' ->
          let ud = uext n2 `shiftL` bitSize n1
              u = uext n3
              (quot, rem) = ud `quotRem` u
          in s { stack = Val (fromIntegral quot) : Val (fromIntegral rem) : stack'}
-}

{-
doesVariable :: Cell cell => ForthLambda cell
doesVariable = return ()
-}

--doesConstant :: Cell cell => String -> ForthLambda cell
{-
doesConstant name = ensureStack name [isAddress] action where
    action = fetch cellValue name
-}

-- | Words that need to exist, but do not have any defined behavior in the Haskell
--   emulated Forth as it is implemented in a different way. It will need a target
--   version though.
-- doNotCall :: Cell cell => String -> ForthLambda cell
doNotCall name =
    liftIO $ hPutStrLn stderr (show name ++ " not to be executed (intended for target)")

-- | Pick up next item in the colon definition which should be a literal and push
--   it on stack.
-- lit :: Cell cell => String -> ForthLambda cell
{-
lit name = modify $ \s ->
               case ip s of
                 [] -> s  -- TODO why is this needed?
                 Literal val : ip' -> s { ip = ip', stack = val : stack s }
-}

-- | Compile a literal.
{-
literal name = ensureStack name [isAny] action where
    action = do
      loadLit <- loadLiteralWordRef
      compileWord loadLit
      lit <- popLiteral
      compileWord (Literal lit)
-}

--    let Literal val : ip' = ip s
--    in s { ip = ip', stack = val : stack s })

{-
toBody :: Cell cell => ForthLambda cell i
toBody = ensureStack [isExecutionToken] $ \s ->
    let ExecutionToken key : stack' = stack s
    in case Map.lookup key (wordKeyMap s) of
         Just word ->
             case dataField word of
               Just field -> s { stack = Address (BodyAdr key 0) : stack' }
-}

{-
compileComma :: Cell cell => String -> ForthLambda cell
compileComma name = ensureStack name [isExecutionToken] action where
    action = do
      ref <- StateT (\s ->
                 let ExecutionToken key : stack' = stack s
                 in return (WordRef key, s { stack = stack' }))
      compileWord ref

comma :: Cell cell => String -> ForthLambda cell
comma name = ensureStack name [isAny] action where
    action = do
      conf <- configuration
      sz <- cellSize
      (val, activeColon, last) <-
          StateT (\s ->
              let val : stack' = stack s
              in return ((val, activeColonDef s, lastWord s), s { stack = stack' }))
      case activeColon of
        Just colonDef -> compileWord (Literal val)
        Nothing ->  -- take care of data field
            case last of
              Just last -> do
                  word <- lookupWord last
                  case word of
                    Just word ->
                        case dataField word of
                            Just field
                                | writable field ->
                                    let field' = (storeData (Cell val) (dataSize field)
                                                            field conf)
                                                 { dataSize = dataSize field + sz }
                                        alter word = Just $ word { dataField = Just field' }
                                    in update (\s ->
                                           s { wordKeyMap =
                                                   Map.update alter last (wordKeyMap s)} )

-}