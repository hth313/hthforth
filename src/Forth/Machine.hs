{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Forth compiler and interpreter basic definitions.

-}

module Forth.Machine (MachineM, ForthLambda, Key(..), Machine(..), ColonElement(..),
                      ColonSlice, ForthWord(..), Body(..), ForthValue(..), ForthValues,
                      StateT(..), Construct(..),
                      liftIO, lift, createVariable, createConstant,
                      wordFromName, initialState, evalStateT, configuration,
                      loadScreens, load, execute, executeColonSlice, pushLiteral, keyName,
                      update, readMachine, addWord, cellSize, executionTokenSize,
                      ensureStack, ensureReturnStack, isValue, isAddress, isAny) where

import Data.Bits
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Control.Monad.State.Lazy hiding (liftIO)
import Forth.Configuration
import Forth.Cell
import Forth.DataField
import Forth.Block
import Forth.Cell
import Numeric
import System.IO

type MachineM cell = StateT (Machine cell) (ConfigurationM cell)

configuration :: Cell cell => (MachineM cell) (Configuration cell)
configuration = lift ask

update :: Cell cell => (Machine cell -> Machine cell) -> ForthLambda cell
update f = StateT (\s -> return ((), f s))

readMachine f = StateT (\s -> return (f s, s))

initialState parser =
    Machine [] [] Map.empty Map.empty [] Nothing Map.empty parser (map Key [1..])

keyName key = StateT (\s ->
    case Map.lookup key (wordKeyMap s) of
      Just word -> return (wordName word, s)
      Nothing -> return (show key, s))

-- | Add a new Forth word
addWord :: Cell cell => (Key -> ForthWord cell) -> ForthLambda cell
addWord word = update (\s ->
    let (key : keys') = keys s
        word' = word key
        finalWord = word' { body = fmap (compileStructure key) (body word') }
    in s { wordKeyMap = Map.insert key finalWord (wordKeyMap s),
           wordNameMap = Map.insert (wordName finalWord) key (wordNameMap s),
           keys = keys' })

-- Compile structures like IF-ELSE-THEN into branch primitives
compileStructure key body@(Code _ _ (Just colon)) =
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
    in body { colon = Just (IntMap.elems colon') }
compileStructure key body@(Code _ _ Nothing) = body
compileStructure key body@(Data _) = body

createVariable name = do
  sz <- cellSize
  addWord $ ForthWord name False (Just (Data (allot sz)))

createConstant name = ensureStack "CONSTANT" [isAny] action where
    action = do
      sz <- cellSize
      val <- StateT (\s -> return (head (stack s), s { stack = tail (stack s) } ))
      addWord $ ForthWord name False (Just (Data (allot sz)))

-- | Given the name of a word, look it up in the dictionary and return its
--   compiled inner representation
wordFromName :: Cell cell => String -> (MachineM cell) (Maybe (ColonElement cell))
wordFromName name =
    StateT (\s ->
        case Map.lookup name (wordNameMap s) of
          Just wordkey -> return (Just $ WordRef wordkey, s)
          Nothing ->
              let literal n = return (Just $ Literal (Val n), s)
              in case readDec name of
                   [(n,"")] -> literal n
                   otherwise ->
                       case readSigned readDec name of
                         [(n,"")] -> literal n
                         otherwise -> return (Nothing, s))

cellSize :: Cell cell => (MachineM cell) cell
cellSize = accessConfigurationSize bytesPerCell

executionTokenSize :: Cell cell => (MachineM cell) cell
executionTokenSize = accessConfigurationSize bytesPerExecutionToken

accessConfigurationSize accessor = do
  lift ask >>= return.accessor

-- | Load an entire set of screens from a file
loadScreens :: Cell cell => FilePath -> (MachineM cell) ()
loadScreens filepath = do
    (blocks, shadows) <-
        lift $ liftIO $ readBlockFile "/Users/hth/projects/CalcForth/src/lib/core.fth"
    --liftIO $ putStrLn (show blocks)
    update (\s -> s { screens = blocks })

-- | Load a screen
load :: Cell cell => Int -> (MachineM cell) (Either String ())
load n = do
  (text, parser) <- readMachine (\s -> (Map.lookup n (screens s), forthParser s))
  lift $ liftIO $ putStr $ (show n) ++ " " -- show loading
  case text of
    Just text -> do
          result <- parser ("screen " ++ show n) text
          return result
    Nothing -> return $ Left ("Screen does not exist: " ++ show n)

-- | Execute the given word
execute :: Cell cell => Key -> (MachineM cell) ()
execute key = do
--  name <- keyName key
--  liftIO $ putStrLn $ "Execute " ++ name
  word <- StateT (\s -> return (Map.lookup key (wordKeyMap s), s))
  case word of
    Just word -> case body word of
                   Just (Code (Just lambda) _ _) -> lambda >> next
                   Just (Code _ _ (Just colonSlice)) ->
                       executeColonSlice colonSlice
                   Just (Data _) -> do
                       update (\s -> s { stack = Address key 0 : stack s} )
                       next

executeColonSlice colonSlice = do
    update (\s -> s { ip = colonSlice, rstack = Continuation (ip s) : rstack s })
    next

-- | Execute next word in colon definition
next :: Cell cell => (MachineM cell) ()
next = do
--  lst <- readMachine ip
--  liftIO $ putStrLn (show lst)
  action <- StateT (\s -> case ip s of
                            [] -> case rstack s of
                                    Continuation slice : rstack' ->
                                        return (next, s { rstack = rstack', ip = slice })
                                    otherwise -> return (return (), s)
                            WordRef key : ip' -> return (execute key, s { ip = ip' })
                            Literal val : ip' ->
                                return (next, s { stack = val : stack s, ip = ip' })
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
                             -- Most recent word, or word being defined
                             lastWord :: Maybe Key,
                             screens :: Map Int String,
                             forthParser :: String -> String -> (MachineM cell) (Either String ()),
                             --                              inputStream :: String,
                             -- Unique identities for words
                             keys :: [Key] }

-- Key type to identify a Forth word internally
newtype Key = Key Int deriving (Show, Eq, Ord)

-- An element of a colon definition
data Cell cell => ColonElement cell = WordRef Key |
                                      Literal (ForthValue cell) |
                                      Structure Construct |
                                      CondBranch Bool Destination |
                                      Branch Destination |
                                      Begin Destination |
                                      While Destination |
                                      NOP -- empty placeholder
                                      deriving (Show, Eq)
type Destination = (Key, Int)  -- word and offset from current location

data Construct = IF | ELSE | THEN | BEGIN | WHILE | REPEAT | UNTIL deriving (Show, Eq)

-- The contents of a colon definition body
type (ColonSlice cell)  = [ColonElement cell]

-- A Forth word, contains the header and its associated body
data Cell cell => ForthWord cell = ForthWord { wordName :: String,
                                               immediate :: Bool,
                                               body :: Maybe (Body cell),
                                               wordKey :: Key }

-- The body of a Forth word
data Cell cell => Body cell = Code { -- native Haskell version
                                     lambda :: Maybe (ForthLambda cell),
                                     -- Native code for a given target
                                     targetNative :: Maybe [Instruction],
                                     -- Colon definition version
                                     colon :: Maybe (ColonSlice cell) } |
                              Data (DataField cell)

-- Values that can be stored on the stack
data Cell cell => ForthValue cell = Continuation (ColonSlice cell)  |
                                    Address Key cell | Val cell |
                                    UndefinedValue deriving (Eq, Show)
type (ForthValues cell) = [ForthValue cell]

-- A Forth native lambda should obey this signature
type ForthLambda cell = (MachineM cell) ()

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
      then lift $ liftIO $ hPutStrLn stderr ("Empty stack for " ++ name)
      else
          let pairs = (zip preds s)
              vals = map (\(f,a) -> f a) pairs
          in if and vals
                then action
                else lift $ liftIO $ hPutStrLn stderr ("Bad stack argument for " ++ name ++
                                                       ", stack is " ++ show (map snd pairs))

isValue (Val _) = True
isValue _ = False
isAddress (Address _ _) = True
isAddress _ = False
isAny = const True
