{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}

module Forth.Machine (MachineM, ForthLambda, Machine(..), push, pop,
                      ForthWord(..), StateT(..), emptyStack, abortWith,
                      initialState, evalStateT, addNative,
                      wordBuffer) where

import Control.Exception
import Control.Monad.State.Lazy
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import Data.Int
import Data.Bits
import Data.Maybe
import Data.Typeable
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Forth.DataField
import Forth.Word
import Forth.WordId
import Forth.Types
import Numeric
import System.IO
import Control.Exception

type MachineM cell = StateT (Machine cell) IO

-- The Forth state
data Machine cell = Machine { -- The Forth stacks
                              stack, rstack :: [Lit cell],
                              dictionaryHead :: LinkField cell,
                              ip :: IP cell,
                              -- Sequence of identies to allocate from
                              keys :: [WordId],
                              -- Data fields for words that need it. This need
                              -- to be modifiable as we mimic data ram. We
                              -- rely on that the underlaying identity of
                              -- a WordId is an Int here.
                              variables :: IntMap DataField,
                              inputBuffer :: ByteString
    }

-- A Forth native lambda should obey this signature
type ForthLambda cell = MachineM cell ()

data ForthException = ForthException String deriving Typeable

instance Exception ForthException
instance Show ForthException where
    show (ForthException text) = text

-- | Internal words that does not need to be redefinable can share
--   this identity.
pseudoId = 0

-- | WordId used for special purposes
wordBuffer = 1 :: Int

-- The first dynamic word identity
firstId = 2

-- | Pop from data stack
pop :: MachineM cell (Lit cell)
pop = StateT $ \s ->
        case stack s of
          t:ts -> return (t, s { stack = ts })
          [] -> emptyStack

emptyStack = abortWith "empty stack"
abortWith = throw . ForthException

-- | Push a value on data stack
push x = modify $ \s -> s { stack = x : stack s }

{-
lookupWord key = gets $ \s -> Map.lookup key (wordKeyMap s)

lookupWordFromName name = do
  Just [WordRef key] <- wordFromName name
  lookupWord key
-}

-- | Create an initial empty Forth machine state
initialState :: cell -> Machine cell
initialState n =
    Machine [] [] Nothing emptyIP [firstId..] IntMap.empty B.empty

-- | Add a native word to the vocabulary.
addNative :: ByteString -> ForthLambda cell -> MachineM cell ()
addNative name action = modify $ \s ->
    let k:ks = keys s
        word = ForthWord name False (dictionaryHead s) k (const action) Native
    in s { keys = ks,
           dictionaryHead = Just word }

doColon word = modify $ \s ->
    let Colon cb = body word
    in s { rstack = Loc (ip s) : rstack s, ip = IP cb 0 }

{-
keyName key = gets $ \s ->
    case Map.lookup key (wordKeyMap s) of
      Just word -> return (wordName word, s)
      Nothing -> return (show key, s)

find word = gets $ \s ->
    case Map.lookup word (wordNameMap s) of
      Just k -> case Map.lookup k (wordKeyMap s) of
                  Just fw -> fw
-}

{-
newKey :: MachineM cell i WordKey
newKey = StateT (\s ->
             let key : keys' = keys s
             in return (key, s { keys = keys' }))
-}

{-
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
-}
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

{-
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
-}

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

{-
-- | Load an entire set of screens from a file
loadScreens :: FilePath -> MachineM cell i ()
loadScreens filepath = do
    (blocks, shadows) <-
        liftIO $ readBlockFile "/Users/hth/projects/CalcForth/src/lib/core.fth"
    --liftIO $ putStrLn (show blocks)
    modify $ \s -> s { screens = blocks }
-}

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

{-
-- | Push a literal on stack
pushLiteral lit =
    modify $ \s -> s { stack = lit : stack s }

-- | Pop a literal from stack
popLiteral :: MachineM s cell (Lit s cell)
popLiteral = StateT (\s ->
    let val : stack' = stack s
    in return (val, s { stack = stack' }))
-}

-- Temporary, replace with something that can actually represent a slice
-- of assembler code.
--data Instruction = String

{-
  Words related to make sure that the stack contains enough things and kind of
  things a word might expect.
-}

{-
ensureStack, ensureReturnStack :: Show cell =>
    [Lit s cell -> Bool] -> (Machine s cell -> Machine s cell) -> ForthLambda s cell
ensureStack = ensure stack
ensureReturnStack = ensure rstack

ensure :: Show cell => (Machine s cell -> [Lit s cell]) ->
          [Lit s cell -> Bool] ->
          (Machine s cell -> Machine s cell)-> ForthLambda s cell
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
-}
