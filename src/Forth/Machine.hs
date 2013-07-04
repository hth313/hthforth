{-# LANGUAGE OverloadedStrings #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}

module Forth.Machine (MachineM, ForthLambda, Machine(..), lookupWord,
                      ForthWord(..), StateT(..), addWord, modifyLastWord,
                      wordFromName, initialState, evalStateT, find,
                      loadScreens, ensureStack, ensureReturnStack,
                      pushLiteral, popLiteral, keyName,
                      isValue, isAddress, isAny, isExecutionToken,
                      isBodyAddress) where

import Control.Monad.State.Lazy
import Data.ByteString.Char8 (ByteString)
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Forth.Cell
import Forth.DataField
import Forth.Block
import Forth.Cell
import Forth.Word
import Forth.Types
import Numeric
import System.IO
import Control.Exception

type MachineM cell i = StateT (Machine cell i) IO

-- The Forth state
data Machine cell i = Machine { -- The Forth stacks
                                stack, rstack :: [Lit cell],
                                -- The dictionary defines the order in which
                                -- words have been defined. Actual lookup is
                                -- done using wordKeyMap.
                                dictionary :: Dictionary,
                                -- Word lookup maps
                                wordKeyMap :: Map WordKey (ForthWord cell i),
                                wordNameMap :: Map ByteString WordKey,
                                -- The interpretive pointer
                                ip :: Lit cell,
                                -- Most recent word
                                lastWord :: Maybe WordKey,
                                -- All forth screens available
                                screens :: IntMap ByteString,
                                activeColonDef :: Maybe (ForthWord cell i),
                                activeColonBody :: [ColonElement cell],
                                --                              inputStream :: String,
                                -- Unique identities for words
                                keys :: [WordKey],
                                variables :: Map WordKey (DataField cell),
                                inputBuffer :: ByteString
    }


-- A Forth native lambda should obey this signature
type ForthLambda cell i = MachineM cell i ()

lookupWord key = gets $ \s -> Map.lookup key (wordKeyMap s)

lookupWordFromName name = do
  Just [WordRef key] <- wordFromName name
  lookupWord key

initialState :: Cell cell => cell -> Machine cell i
initialState n =
    Machine [] [] [] Map.empty Map.empty (Address DeadAdr) Nothing IntMap.empty
            Nothing [] (map WordKey [1..]) Map.empty ""

keyName key = gets $ \s ->
    case Map.lookup key (wordKeyMap s) of
      Just word -> return (wordName word, s)
      Nothing -> return (show key, s)

find word = gets $ \s ->
    case Map.lookup word (wordNameMap s) of
      Just k -> case Map.lookup k (wordKeyMap s) of
                  Just fw -> fw


{-
newKey :: MachineM cell i WordKey
newKey = StateT (\s ->
             let key : keys' = keys s
             in return (key, s { keys = keys' }))
-}

-- | Add a new Forth word
--addWord :: ForthWord cell -> ForthLambda cell
addWord word = modify $ \s ->
    let key : keys' = keys s
        finalWord = word key
    in s { wordKeyMap = Map.insert key finalWord (wordKeyMap s),
           wordNameMap = Map.insert (wordName finalWord) key (wordNameMap s),
           lastWord = Just key,
           dictionary = key : dictionary s }

modifyLastWord f = modify $ \s ->
    let key = head (dictionary s)
    in s { wordKeyMap = Map.update f key (wordKeyMap s) }

{-
-- Compile structures like IF-ELSE-THEN into branch primitives
compileStructure key colon =
    let numbered = zip [1..] colon
        visit (stack, colon) (n, Structure IF) = ((n, CondBranch False) : stack, colon)
        visit ((nif, branch) : stack, colon) (nelse, Structure ELSE) =
              let colon' = IntMap.update (const (Just (branch (key, nelse + 1)))) nif colon
              in ((nelse, Branch) : stack, colon')
        visit ((n, branch) : stack, colon) (nthen, Structure THEN) =
              let colon' = IntMap.update (const (Just (branch (key, nthen + 1)))) n colon
                  colon'' = IntMap.update (const (Just NOP)) nthen colon'
              in (stack, colon'')
        visit (stack, colon) (n, Structure BEGIN) =
            let colon' = IntMap.update (const (Just NOP)) n colon'
            in ((n, Begin) : stack, colon')
        visit (stack, colon) (n, Structure WHILE) =  ((n, While) : stack, colon)
        visit ((nwhile, while) : (nbegin, begin) : stack, colon) (n, Structure REPEAT)
              | while x == While x && begin x == Begin x =
                  let colon' = IntMap.update (const (Just (Branch (key, nbegin)))) n colon
                      colon'' = IntMap.update (const (Just (CondBranch False (key, n + 1)))) nbegin colon'
                  in (stack, colon'')
        visit ((nbegin, begin) : stack, colon) (n, Structure UNTIL)
              | begin x == Begin x =
                  let colon' = IntMap.update (const (Just (CondBranch False (key, nbegin)))) n colon
                  in (stack, colon')
        visit acc ins = acc
        x = (key, 0)
        (stack', colon') = foldl visit ([], IntMap.fromList numbered) numbered
        colonInt = IntMap.elems colon' -- TODO: replace with ColonSlice?
    in IntMap.elems colon'
-}
{-
createVariable :: String -> ForthLambda cell
createVariable name = do
  sz <- cellSize
  key <- newKey
  Just does <- lookupWordFromName "_VAR"
  addWord (ForthWord name False key (Just (allot sz)) Nothing Nothing
                     (Just (wordKey does)) Nothing)
  putDP (Address key 0)
-}
{-
createConstant :: String -> ForthLambda cell
createConstant name = ensureStack "CONSTANT" [isAny] action where
    action = do
      sz <- cellSize
      key <- newKey
      Just does <- lookupWordFromName "_CON"
      val <- StateT (\s -> return (head (stack s), s { stack = tail (stack s) } ))
      conf <- configuration
      let field = storeData (Cell val) 0 (allot sz) conf
      addWord (ForthWord name False key (Just field) Nothing Nothing
                         (Just (wordKey does)) Nothing)
      putDP (Address key 0)
-}

-- | Set dictionary pointer. This need to ensure that DP exists, which it does not
--   initially.
{-
putDP lit = do
  dp <- wordFromName "DP"
  when (isJust dp) (pushLiteral lit >>  executeSlice [ "DP", "!" ])
-}

-- | Given the name of a word, look it up in the dictionary and return its
--   compiled inner representation
wordFromName :: Integral cell => String -> MachineM cell i (Maybe [ColonElement cell])
wordFromName name = do
  StateT (\s ->
      case Map.lookup name (wordNameMap s) of
        Just wordkey -> return (Just [WordRef wordkey], s)
        Nothing ->
          let literal n = return (Just $ [lit, Literal (Val n)], s)
              lit = case Map.lookup "_LIT" (wordNameMap s) of
                      Just ref -> WordRef ref
          in case readDec name of
               [(n,"")] -> literal n
               otherwise ->
                   case readSigned readDec name of
                     [(n,"")] -> literal n
                     otherwise -> return (Nothing, s))

{-
loadLiteralWordRef :: MachineM cell i (ColonElement cell)
loadLiteralWordRef = do
  loadLit <- wordFromName "_LIT"
  case loadLit of
    Just [loadLit] -> return loadLit
-}

{-
-- | Perform interpretation or compilation semantics for the given word.
perform :: String -> MachineM cell Bool
perform name = do
  -- check compilation or execution state
  stateWord <- lookupWordFromName "STATE"
  let field = case stateWord of
                Just word ->
                    case dataField word of
                      Just field -> field
  let Cell (Val stateVal) = fetchData 0 field
  -- Dispatch on state and either execute or compile the word
  word <- wordFromName name
  case word of
    Just words -> do
        mapM_ perform1 words
        return True
        where
          perform1 ref@(WordRef key) = do
              word <- lookupWord key
              if stateVal == 0 || maybe False immediate word
                 then execute key
                 else compileWord ref
          perform1 lit@(Literal val)
              | stateVal == 0 = pushLiteral val
              | otherwise = compileWord lit
    Nothing -> return False
-}

{-
openColonDef name = do
  key <- newKey
  StateT (\s ->
      return ((), s { activeColonDef = Just (ForthWord name False key Nothing Nothing
                                                       Nothing Nothing Nothing),
                      activeColonBody = [] }))
  putDP (ColonAddress key 0)
-}

{-
closeColonDef :: ForthLambda cell
closeColonDef = do
    (def, body) <- StateT (\s -> return ((activeColonDef s, activeColonBody s),
                                         s { activeColonDef = Nothing } ))
    case def of
      Just def -> do
          addWord $ def { body = Just body }
-}

{-
compileWord :: ColonElement cell -> ForthLambda cell
compileWord elt = StateT (\s ->
    return ((), s { activeColonBody = activeColonBody s ++ [elt] }))
-}


--cellSize :: Cell cell => MachineM cell i cell
--cellSize = accessConfigurationSize bytesPerCell

--instructionSize :: Cell cell => MachineM cell i cell
--instructionSize = accessConfigurationSize bytesPerInstruction

--accessConfigurationSize accessor = do
--  configuration >>= return.accessor

-- | Load an entire set of screens from a file
loadScreens :: FilePath -> MachineM cell i ()
loadScreens filepath = do
    (blocks, shadows) <-
        liftIO $ readBlockFile "/Users/hth/projects/CalcForth/src/lib/core.fth"
    --liftIO $ putStrLn (show blocks)
    modify $ \s -> s { screens = blocks }

-- | Load a screen
{-
load :: Int -> MachineM cell (Either String ())
load n = do
  -- TODO: push input source on stack
  buftext <- gets $ IntMap.lookup n . screens
  liftIO $ putStr $ (show n) ++ " " -- show loading
  case buftext of
    Just text -> do
          result <- parser ("screen " ++ show n) text
          return result
    Nothing -> return $ Left ("Screen does not exist: " ++ show n)
-}

-- | Execute the given word
{-
execute :: WordKey -> MachineM cell ()
execute key = do
--  name <- keyName key
--  liftIO $ putStrLn $ "Execute " ++ name
  word <- lookupWord key
  case word of
    Just (ForthWord _ _ _ (Just field) _ _ (Just cfa) _) -> do
        -- Push address on stack
        update (\s -> s { stack = Address key 0 : stack s} )
        execute cfa
    Just (ForthWord _ _ _ _ (Just lambda) _ _ _) -> lambda >> next
    Just (ForthWord _ _ _ _ _ _ _ (Just colonSlice)) ->
        executeColonSlice colonSlice
-}

-- | Given a list of Forth words (as strings), compile it to a slice and execute it
{-
executeSlice :: [String] -> ForthLambda cell
executeSlice list =
    let compile name = do
          word <- wordFromName name
          case word of
            Just ref -> return ref
            Nothing -> throw $ ErrorCall ("Failed to find: " ++ show name)
    in do
      slice <- mapM compile list
      executeColonSlice (concat slice)
-}

{-
executeColonSlice colonSlice = do
    update (\s -> s { ip = colonSlice, rstack = Continuation (ip s) : rstack s })
    next
-}

{-
-- | Execute next word in colon definition
next :: MachineM cell ()
next = do
--  lst <- readMachine ip
--  liftIO $ putStrLn (show lst)
  action <- StateT (\s -> case ip s of
                            [] -> case rstack s of
                                    Continuation slice : rstack' ->
                                        return (next, s { rstack = rstack', ip = slice })
                                    otherwise -> return (return (), s)
                            WordRef key : ip' -> return (execute key, s { ip = ip' })
-- This one should be loaded by a preceeding _LIT
--                            Literal val : ip' ->
--                                return (next, s { stack = val : stack s, ip = ip' })
                   )
  action
-}

-- | Push a literal on stack
pushLiteral lit =
    modify $ \s -> s { stack = lit : stack s }

-- | Pop a literal from stack
popLiteral :: MachineM cell i (Lit cell)
popLiteral = StateT (\s ->
    let val : stack' = stack s
    in return (val, s { stack = stack' }))

-- Temporary, replace with something that can actually represent a slice
-- of assembler code.
data Instruction = String

{-
  Words related to make sure that the stack contains enough things and kind of
  things a word might expect.
-}

ensureStack, ensureReturnStack ::
    Show cell => [Lit cell -> Bool] -> (Machine cell i -> Machine cell i) -> ForthLambda cell i
ensureStack = ensure stack
ensureReturnStack = ensure rstack

ensure :: Show cell => (Machine cell i -> [Lit cell]) ->
          [Lit cell -> Bool] ->
          (Machine cell i -> Machine cell i)-> ForthLambda cell i
ensure stack preds action = do
  s <- gets stack
  if length preds > length s
      then liftIO $ hPutStrLn stderr "empty stack"
      else
          let pairs = (zip preds s)
              vals = map (\(f,a) -> f a) pairs
          in if and vals
                then modify action
                else liftIO $ hPutStrLn stderr ("bad stack argument, stack is " ++
                                                show (map snd pairs))

isValue (Val _) = True
isValue _ = False
isAddress Address{} = True
isAddress _ = False
isBodyAddress (Address BodyAdr{}) = True
isBodyAddress _ = False
isAny = const True
isExecutionToken (ExecutionToken _) = True
isExecutionToken _ = False
