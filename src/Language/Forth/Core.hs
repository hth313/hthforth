{-# LANGUAGE ScopedTypeVariables, OverloadedStrings, PatternGuards #-}
{-|

  Forth core words in native lambdas.

-}

module Language.Forth.Core (addNatives, abort, quit) where

import Control.Applicative
import Control.Exception (try)
import Control.Monad.State.Lazy hiding (state)
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import qualified Data.Vector.Storable.ByteString.Char8 as C
import qualified Data.Vector as V
import Data.Bits
import Language.Forth.Address
import Language.Forth.Cell
import Language.Forth.CellMemory
import Language.Forth.DataField
import Language.Forth.Machine
import Language.Forth.StreamFile
import Language.Forth.Types
import Language.Forth.Word
import Util.Memory
import qualified Data.IntMap as IntMap
import System.IO
import Numeric
import Prelude hiding (drop)
import System.Exit
import System.Console.Haskeline



-- | Populate the vocabulary with a basic set of Haskell native words.
addNatives :: Cell cell => MachineM cell ()
addNatives = do
  addNative "BYE" (liftIO $ exitSuccess)
  addNative "+"     plus
  addNative "-"   $ binary (-)
  addNative "*"   $ binary (*)
  addNative "/"   $ binary divide
  addNative "<"   $ binary (boolVal $ (<))
  addNative "="   $ binary (bool $ (==))
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
  addNative "R>"    rto
  addNative "R@"    rfetch
  addNative "I"     rfetch
  addNative ">R"    tor
  addNative "FIND"  find
  addVar    B.empty inputBufferId Nothing
  addVar    "INPUT-LINE" inputBufferPtrId (Just $ Val 0)
  addVar    "#INPUT-LINE" inputBufferLengthId (Just $ Val 0)
  addVar    "STATE" stateId (Just $ Val 0)
  addVar    "SOURCE-ID" sourceId (Just $ Val 0)
  addVar    ">IN" toInId  (Just $ Val 0)
  addNative "EVALUATE" evaluate
  addNative "LOAD-SOURCE" xloadSource
  addNative "\\" backslash >> makeImmediate
  addNative "SMUDGE" smudge
  addNative "CREATE" ((\name -> create name doVar) =<< parseName)
  addNative ":" colon
  addNative ";" semicolon >> makeImmediate
  addNative "IMMEDIATE" makeImmediate
  addNativeFixed exitId "EXIT" colonExit
  addNative "CONSTANT" constant
  addNative "TRUE"  (push $ Val (-1))
  addNative "FALSE" (push $ Val 0)
  addNative "," comma
  addNative "HERE" here
  addNative "(LIT)" lit
  addNative "IF" xif >> makeImmediate
  addNative "ELSE" xelse >> makeImmediate
  addNative "THEN" xthen >> makeImmediate
  addNative "JUMP-FALSE" jumpFalse
  addNative "JUMP" jump
  addNative "DO" xdo >> makeImmediate
  addNative "LOOP" loop >> makeImmediate
  addNative "+LOOP" plusLoop >> makeImmediate
  addNative "(DO)" rdo
  addNative "(LOOP)" rloop
  addNative "(+LOOP)" rplusLoop
  addNative "LEAVE" leave
  addNative "." (pop >>= (liftIO . putStrLn . show)) -- temporary
      where
        divide (Val a) (Val b) = Val (a `div` b)
        divide a b = Bot $ show a ++ " / " ++ show b
        boolVal p (Val a) (Val b) = Val $ if p a b then (-1) else 0
        bool p a b = Val $ if p a b then (-1) else 0

plus, dup, drop, swap, over, rot, plusStore,
      tor, rto, rfetch :: Cell cell => ForthLambda cell
plus = binary (+)

dup = updateState $ \s ->
    case stack s of
      s0 : ss -> newState s { stack = s0 : s0 : ss }
      otherwise -> emptyStack s

drop = updateState $ \s ->
    case stack s of
      s0 : ss -> newState s { stack = ss }
      otherwise -> emptyStack s

swap = updateState $ \s ->
    case stack s of
      s0 : s1 : ss -> newState s { stack = s1 : s0 : ss }
      otherwise -> emptyStack s

over = updateState $ \s ->
    case stack s of
      s0 : s1 : ss -> newState s { stack = s1 : s0 : s1 : ss }
      otherwise -> emptyStack s

rot = updateState $ \s ->
    case stack s of
      s0 : s1 : s2 : ss -> newState s { stack = s2 : s0 : s1 : ss }
      otherwise -> emptyStack s

plusStore = dup >> fetch >> rot >> plus >> swap >> store

tor = updateState $ \s ->
    case stack s of
      s0 : ss -> newState s { stack = ss, rstack = s0 : rstack s }
      otherwise -> emptyStack s

rto = updateState $ \s ->
    case rstack s of
      r0 : rs -> newState s { stack = r0 : stack s, rstack = rs }
      otherwise -> emptyStack s

rfetch = updateState $ \s ->
    case rstack s of
      r0 : rs -> newState s { stack = r0 : stack s }
      otherwise -> emptyStack s

-- | Perform a binary operation
binary op = updateState $ \s ->
    case stack s of
      s0 : s1 : ss -> newState s { stack = s1 `op` s0 : ss  }
      otherwise -> emptyStack s


inputBuffer, inputBufferPtr, inputBufferLength, toIn,
    sourceID, state :: Cell cell => ForthLambda cell
inputBuffer = wordIdExecute inputBufferId
inputBufferPtr = wordIdExecute inputBufferPtrId
inputBufferLength = wordIdExecute inputBufferLengthId
toIn = wordIdExecute toInId
sourceID = wordIdExecute sourceId
state = wordIdExecute stateId


fetch :: Cell cell => ForthLambda cell
fetch = updateState $ \s ->
    case stack s of
      Address (Just adr@(Addr wid _)) : rest ->
          case IntMap.lookup wid (variables s) of
            Just (DataField cm) ->
                case readCell adr cm of
                  Just x -> newState s { stack = x : rest }
                  otherwise -> abortWith "@ outside data field" s
            Just (BufferField mem) -> abortWith "@ in buffer field" s
            Nothing -> case IntMap.lookup wid (wordMap s) of
                         Just word -> abortWith (C.unpack (name word) ++ " lacks data field") s
                         Nothing -> abortWith "@ on unknwon word" s
      [] -> emptyStack s
      a : _ -> abortWith ("cannot fetch from " ++ show a) s

cfetch :: Cell cell => ForthLambda cell
cfetch = modify $ \s ->
    case stack s of
      Address (Just adr@(Addr wid i)) : rest
          | Just (BufferField bm) <- IntMap.lookup wid (variables s) ->
              let c = Val $ fromIntegral $ B.index (chunk bm) i
              in s { stack = c : rest }


store :: Cell cell => ForthLambda cell
store = updateState $ \s ->
    case stack s of
      Address (Just adr@(Addr wid i)) : val : rest
          | Just (DataField cm) <- IntMap.lookup wid (variables s) ->
              newState s { variables = IntMap.insert wid (DataField $ writeCell val adr cm)
                                       (variables s),
                           stack = rest }
      [] -> emptyStack s
      [x] -> abortWith ("no value to store to " ++ show x) s
      x:_ -> abortWith ("cannot store to " ++ show x) s


-- | Find the name (counted string) in the dictionary
--   ( c-addr -- c-addr 0 | xt 1 | xt -1 )
find :: Cell cell => ForthLambda cell
find = do
  caddr <- pop
  mword <- searchDictionary =<< countedText caddr
  modify $ \s ->
      case mword of
        Just word
            | immediate word -> s { stack = Val 1 : XT word : (stack s) }
            | otherwise -> s { stack = Val (-1) : XT word : (stack s) }
        Nothing -> s { stack = Val 0 : caddr : stack s }


-- | Parse a name in the input stream. Returns the parsed name (does not
--   put on stack). Uses and updates >IN.
--   This is initially the foundation for parsing Forth words.
--   ( "<spaces>ccc<space>" -- )
parseName :: Cell cell => MachineM cell ByteString
parseName = do
  parseStart
  name <- updateStateVal "" $ \s ->
    case stack s of
       Address (Just (Addr wid off)) : ss
          | Just (BufferField cmem) <- IntMap.lookup wid (variables s) ->
              let start = B.drop off (chunk cmem)
                  (skipCount, nameStart) = skipSpaces start
                  skipSpaces bs
                      | B.null bs = (0, bs)
                      | otherwise = skipSpaces1 0 bs where
                      skipSpaces1 n bs
                          | C.head bs <= ' ' = skipSpaces1 (n + 1) (B.tail bs)
                          | otherwise = (n, bs)
                  name = C.takeWhile (> ' ') nameStart
                  nameLength = B.length name
                  inAdjust = skipCount + nameLength
                  result = Address $ Just (Addr wid (skipCount + off))
              in return (Right name, s { stack = Val (fromIntegral inAdjust) : ss })
       otherwise -> abortWith "parseName failed" s
  toIn >> plusStore
  return name


-- | Copy word from given address with delimiter to a special transient area.
--   ( "<chars>ccc<char>" -- c-addr )
xword :: Cell cell => ForthLambda cell
xword = do
  name <- parseName
  modify $ \s ->
      let countedField = textBuffer wordBufferId (B.cons (fromIntegral $ B.length name) name)
          result = Address (Just $ Addr wordBufferId 0)
      in s { stack = result : stack s,
             variables = IntMap.insert wordBufferId countedField (variables s) }


-- | Processes input text stored in the input buffer.
interpret :: Cell cell => ForthLambda cell
interpret = state >> fetch >> pop >>= interpret1 where
    interpret1 stateFlag =
        let compiling = stateFlag /= Val 0
            parseNumber = parse =<< countedText =<< pop where
                parse bs = case readDec text of
                             [(x,"")]
                                 | compiling -> compileWord "(LIT)" >> compile (Val x)
                                 | otherwise -> push $ Val x
                             otherwise -> abortMessage $ text ++ " ?"
                             where text = C.unpack bs
        in do
          xword
          dup >> cfetch
          eol <- liftM (Val 0 ==) pop
          if eol then drop else do
              find
              val <- pop
              case val of
                Val 0 -> parseNumber >> interpret       -- not found
                Val 1 -> execute >> interpret           -- immediate word
                _ | compiling -> pop >>= compile >> interpret -- normal word found
                  | otherwise -> execute >> interpret


-- | Given a counted string, extract the actual text as an individual ByteString.
countedText (Address (Just (Addr wid off))) = updateStateVal "" $ \s ->
    case IntMap.lookup wid (variables s) of
      Just (BufferField cmem) ->
          let count = fromIntegral $ B.index (chunk cmem) off
          in return (Right $ B.take count $ B.drop (off + 1) (chunk cmem), s)
      otherwise ->
          case IntMap.lookup wid (wordMap s) of
            Just word -> abortWith ("expected address pointing to char buffer for " ++
                                    (C.unpack $ name word)) s
            otherwise -> abortWith "expected address pointing to char buffer" s
countedText _ = abortMessage "expected address" >> return ""


abort :: Cell cell => ForthLambda cell
abort = do
  modify $ \s -> s { stack = [] }
  push (Val $ 0) >> state >> store
  quit


quit :: Cell cell => ForthLambda cell
quit = do
  modify $ \s -> s { rstack = [], ip = Nothing }
  push (Val 0) >> sourceID >> store
  mainLoop


mainLoop :: Cell cell => ForthLambda cell
mainLoop = do
  mline <- lift $ getInputLine ""
  case mline of
    Nothing -> return ()
    Just input ->
        let line = C.pack input
        in do
          putField inputBufferId (textBuffer inputBufferId line)
          push (Val 0) >> toIn >> store
          pushAdr inputBufferId >> inputBufferPtr >> store
          push (Val $ fromIntegral $ B.length line) >> inputBufferLength >> store
          interpret
  liftIO $ putStrLn "ok"
  mainLoop


evaluate :: Cell cell => ForthLambda cell
evaluate = do
  inputBufferPtr >> fetch >> tor        -- save input specification
  inputBufferLength >> fetch >> tor
  push (Val $ -1) >> sourceID >> store  -- set SOURCE-ID to -1
  inputBufferLength >> store            -- set new SOURCE specification
  inputBufferPtr >> store
  push 0 >> toIn >> store               -- clear >IN
  interpret
  rto >> toIn >> store                  -- restore input specification
  rto >> inputBufferPtr >> store


-- | Load a source file using the Forth interpreter
loadSource :: Cell cell => FilePath -> ForthLambda cell
loadSource  filename = do -- withTempBuffer evaluate =<< (liftIO $ readSourceFile filename)
  mc <- liftIO $ try $ readSourceFile filename
  case mc of
    Right contents -> withTempBuffer evaluate contents
    Left (e :: IOException) -> abortMessage $ show e


-- | Load next word in input stream as a source file.
xloadSource :: Cell cell => ForthLambda cell
xloadSource = loadSource =<< liftM C.unpack parseName


-- | Rest of line is comment
backslash :: Cell cell => ForthLambda cell
backslash = do
  parseStart
  mo <- StateT $ \s ->
      case stack s of
        Address (Just (Addr wid off)) : ss
          | Just (BufferField cmem) <- IntMap.lookup wid (variables s) ->
              let start = B.drop off (chunk cmem)
              in return ((off + 1 +) <$> C.findIndex ('\n'==) start, s { stack = ss } )
  case mo of
    Nothing -> inputBufferLength >> fetch
    Just n -> push $ Val (fromIntegral n)
  toIn >> store


-- Push start address of parse area on stack, basically 'SOURCE +'
parseStart :: Cell cell => ForthLambda cell
parseStart = inputBufferPtr >> fetch >> toIn >> fetch >> plus

colon :: Cell cell => ForthLambda cell
colon = do
  parseName >>= flip create doColon
  push (Val $ (-1)) >> state >> store


semicolon :: Cell cell => ForthLambda cell
semicolon = do
  Just exit <- wordLookup exitId
  compile $ XT exit
  smudge
  push (Val 0) >> state >> store


colonExit :: Cell cell => ForthLambda cell
colonExit = updateState $ \s ->
    case rstack s of
      Loc ip : rs -> newState s { ip = ip, rstack = rs }
      otherwise -> abortWith "EXIT - misaligned return stack at" s


constant :: Cell cell => ForthLambda cell
constant = do
  (\name -> create name doConst) =<< parseName
  comma
  smudge

here :: Cell cell => ForthLambda cell
here = modify $ \s ->
    case defining s of
      Just word
          | Colon cb <- body word ->
                s { stack = Address (Just (Addr (wid word) (V.length cb))) : stack s }

lit :: Cell cell => ForthLambda cell
lit = modify $ \s ->
    case ip s of
      Just (IP cb off) -> s { ip = Just (IP cb (off + 1)),
                              stack = (V.!) cb off : stack s }

backpatch :: Cell cell => ForthLambda cell
backpatch = modify $ \s ->
    case defining s of
      Just word
           | (Address (Just (Addr _ loc))) : dest : stack' <- stack s,
             Colon cb <- body word ->
                 s { stack = stack',
                     defining = Just word { body = Colon $ (V.//) cb [(loc + 1, dest)] } }

xif, xelse, xthen :: Cell cell => ForthLambda cell
xif = here >> compileWord "JUMP-FALSE" >> compile (Val 0)
xelse = here >> compileWord "JUMP" >> compile (Val 0) >> here >> rot >> backpatch
xthen = here >> swap >> backpatch

jumpFalse :: Cell cell => ForthLambda cell
jumpFalse = do
  val <- pop
  case val of
    Val n | n == 0 -> jump
    otherwise -> noJump

jump :: Cell cell => ForthLambda cell
jump = modify $ \s ->
    case ip s of
      Just (IP cb off)
          | Address (Just (Addr _ off')) <- (V.!) cb off ->
                s { ip = Just (IP cb off') }

noJump :: Cell cell => ForthLambda cell
noJump = modify $ \s ->
    case ip s of
      Just (IP cb off) -> s { ip = Just (IP cb (off + 1)) }

-- Loops, we use Forth level run-time words here and we will later replace
-- DO-LOOP words with something else.
xdo, rdo, loop, rloop, plusLoop, rplusLoop, back, leave :: Cell cell => ForthLambda cell
xdo = compileWord "(DO)" >> here
rdo = updateState $ \s -> case stack s of
                            s0 : s1 : ss -> newState s { stack = ss, rstack = s0 : s1 : rstack s }
                            otherwise -> emptyStack s

loop = compileLoop "(LOOP)"
rloop = updateState $ rloopHelper (1+)
plusLoop = compileLoop "(+LOOP)"
rplusLoop = updateState $ \s -> case stack s of
                                  n : ss -> rloopHelper (n+) (s { stack = ss })
rloopHelper f s = case rstack s of
                    i : limit : rs ->
                        let i' = f i
                        in newState $ if i' < limit
                                      then s { rstack = i' : limit : rs,
                                               stack = Val 0 : stack s }
                                      else s { rstack = rs,
                                               stack = Val (-1) : stack s }
                    otherwise -> emptyStack s
compileLoop rt = compileWord rt >> compileWord "JUMP-FALSE" >> back
back = pop >>= compile
leave = updateState $ \s -> case rstack s of
                              _i : limit : rs -> newState s { rstack = limit : limit : rs }
                              otherwise -> emptyStack s
