{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}

module Forth.Machine (MachineM, ForthLambda, Machine(..),
                      ForthWord(..), StateT(..), newKey, loadLiteralWordRef,
                      createVariable, createConstant, perform, compileWord,
                      wordFromName, initialState, evalStateT, configuration,
                      loadScreens, load, execute, executeSlice, executeColonSlice,
                      pushLiteral, keyName, openColonDef, closeColonDef,
                      update, readMachine, addWord, cellSize, executionTokenSize,
                      ensureStack, ensureReturnStack, isValue, isAddress, isAny) where

import Data.Int
import Data.Bits
import Data.Maybe
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State.Lazy
import Forth.Configuration
import Forth.Cell
import Forth.DataField
import Forth.Block
import Forth.Cell
import Forth.Types
import Numeric
import System.IO
import Control.Exception

type MachineM cell = StateT (Machine cell) (ConfigurationM cell)

configuration :: Cell cell => MachineM cell (Configuration cell)
configuration = lift ask

update :: Cell cell => (Machine cell -> Machine cell) -> ForthLambda cell
update f = StateT (\s -> return ((), f s))

readMachine f = StateT (\s -> return (f s, s))

lookupWord key = StateT (\s -> return (Map.lookup key (wordKeyMap s), s))

lookupWordFromName name = do
  Just [WordRef key] <- wordFromName name
  lookupWord key

initialState parser =
    Machine [] [] Map.empty Map.empty [] Nothing Map.empty parser
            Nothing [] (map Key [1..])

keyName key = StateT (\s ->
    case Map.lookup key (wordKeyMap s) of
      Just word -> return (wordName word, s)
      Nothing -> return (show key, s))

newKey :: Cell cell => MachineM cell Key
newKey = StateT (\s ->
             let key : keys' = keys s
             in return (key, s { keys = keys' }))

-- | Add a new Forth word
addWord :: Cell cell => ForthWord cell -> ForthLambda cell
addWord word = update (\s ->
    let finalWord = word { body = fmap (compileStructure key) (body word) }
        key = wordKey finalWord
    in s { wordKeyMap = Map.insert key finalWord (wordKeyMap s),
           wordNameMap = Map.insert (wordName finalWord) key (wordNameMap s),
           lastWord = Just key } )

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

createVariable :: Cell cell => String -> ForthLambda cell
createVariable name = do
  sz <- cellSize
  key <- newKey
  Just does <- lookupWordFromName "_VAR"
  addWord (ForthWord name False key (Just (allot sz)) Nothing Nothing
                     (Just (wordKey does)) Nothing)

createConstant :: Cell cell => String -> ForthLambda cell
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

-- | Given the name of a word, look it up in the dictionary and return its
--   compiled inner representation
wordFromName :: Cell cell => String -> MachineM cell (Maybe [ColonElement cell])
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

loadLiteralWordRef :: Cell cell => MachineM cell (ColonElement cell)
loadLiteralWordRef = do
  loadLit <- wordFromName "_LIT"
  case loadLit of
    Just [loadLit] -> return loadLit

-- | Perform interpretation or compilation semantics for the given word.
perform :: Cell cell => String -> MachineM cell Bool
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

openColonDef name = do
  key <- newKey
  StateT (\s ->
      return ((), s { activeColonDef = Just (ForthWord name False key Nothing Nothing
                                                       Nothing Nothing Nothing),
                      activeColonBody = [] }))

closeColonDef :: Cell cell => ForthLambda cell
closeColonDef = do
    (def, body) <- StateT (\s -> return ((activeColonDef s, activeColonBody s), s))
    case def of
      Just def -> addWord $ def { body = Just body }

compileWord :: Cell cell => ColonElement cell -> ForthLambda cell
compileWord elt = StateT (\s ->
    return ((), s { activeColonBody = activeColonBody s ++ [elt] }))

cellSize :: Cell cell => MachineM cell cell
cellSize = accessConfigurationSize bytesPerCell

executionTokenSize :: Cell cell => MachineM cell cell
executionTokenSize = accessConfigurationSize bytesPerExecutionToken

accessConfigurationSize accessor = do
  configuration >>= return.accessor

-- | Load an entire set of screens from a file
loadScreens :: Cell cell => FilePath -> MachineM cell ()
loadScreens filepath = do
    (blocks, shadows) <-
        liftIO $ readBlockFile "/Users/hth/projects/CalcForth/src/lib/core.fth"
    --liftIO $ putStrLn (show blocks)
    update (\s -> s { screens = blocks })

-- | Load a screen
load :: Cell cell => Int -> MachineM cell (Either String ())
load n = do
  (text, parser) <- readMachine (\s -> (Map.lookup n (screens s), forthParser s))
  liftIO $ putStr $ (show n) ++ " " -- show loading
  case text of
    Just text -> do
          result <- parser ("screen " ++ show n) text
          return result
    Nothing -> return $ Left ("Screen does not exist: " ++ show n)

-- | Execute the given word
execute :: Cell cell => Key -> MachineM cell ()
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

-- | Given a list of Forth words (as strings), compile it to a slice and execute it
executeSlice :: Cell cell => [String] -> ForthLambda cell
executeSlice list =
    let compile name = do
          word <- wordFromName name
          case word of
            Just ref -> return ref
            Nothing -> throw $ ErrorCall ("Failed to find: " ++ show name)
    in do
      slice <- mapM compile list
      executeColonSlice (concat slice)

executeColonSlice colonSlice = do
    update (\s -> s { ip = colonSlice, rstack = Continuation (ip s) : rstack s })
    next

-- | Execute next word in colon definition
next :: Cell cell => MachineM cell ()
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

-- | Push a literal on stack
pushLiteral lit =
    update (\s -> s { stack = lit : stack s })

-- The Forth state
data Cell cell =>
    Machine cell = Machine { -- The Forth stacks
                             stack, rstack :: ForthValues cell,
                             -- Word lookup maps
                             wordKeyMap :: Map Key (ForthWord cell),
                             wordNameMap :: Map String Key,
                             -- The interpretive pointer
                             ip :: ColonSlice cell,
                             -- Most recent word
                             lastWord :: Maybe Key,
                             screens :: Map Int String,
                             forthParser :: String -> String -> MachineM cell (Either String ()),
                             activeColonDef :: Maybe (ForthWord cell),
                             activeColonBody :: [ColonElement cell],
                             --                              inputStream :: String,
                             -- Unique identities for words
                             keys :: [Key] }

-- A Forth word, contains the header and its associated body
data Cell cell => ForthWord cell = ForthWord { wordName :: String,
                                               immediate :: Bool,
                                               wordKey :: Key,
                                               -- For data words.
                                               dataField :: Maybe (DataField cell),
                                               -- Native Haskell implementation
                                               lambda :: Maybe (ForthLambda cell),
                                               -- Native target code
                                               targetNative :: Maybe [Instruction],
                                               -- Forth level run-time semantics for
                                               -- this word. This will refer to a
                                               -- nameless word that implements the
                                               -- DOES> part. If invoked, the dataField
                                               -- is first to be pused on the stack
                                               cfa :: Maybe Key,
                                               -- Colon definition for this word
                                               body :: Maybe (ColonSlice cell) }

-- A Forth native lambda should obey this signature
type ForthLambda cell = MachineM cell ()

-- Temporary, replace with something that can actually represent a slice
-- of assembler code.
data Instruction = String

{-
  Words related to make sure that the stack contains enough things and kind of
  things a word might expect.
-}

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
