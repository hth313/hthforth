{-# LANGUAGE FlexibleInstances, LambdaCase, MultiParamTypeClasses, TypeSynonymInstances #-}
{-# LANGUAGE MultiWayIf #-}
{- |

   The Forth interpreter.

-}

module Language.Forth.Interpreter (initialState, quit) where

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Vector as V
import Language.Forth.Cell
import Language.Forth.CellVal
import Language.Forth.Dictionary
import Language.Forth.Interpreter.Monad
import Language.Forth.Primitive
import Language.Forth.Target

initialState :: Cell cell => Target cell -> FState cell
initialState target = FState [] [] [] target newDictionary

-- | Foundation of the Forth interpreter
instance Cell cell => Primitive (CV cell) (FMonad cell ()) where
  semi = call =<< rpop
  execute = call =<< dpop
  lit val = dpush val >> next
  swap = updateState $ \s -> case stack s of
                               s0 : s1 : ss -> newState s { stack = s1 : s0 : ss }
                               otherwise -> emptyStack s
  add = (dpush =<< (liftM2 (+) dpop dpop)) >> next
  quit = liftIO (putStrLn " ok") >> next
  interpret = return ()
  docol (x:xs) = modify (\s -> s { ip = xs }) >> x
  docol [] = semi
  branch = docol
  branch0 loc = dpop >>= \n -> if | isZero n -> docol loc
                                  | otherwise  -> next

abort :: Cell cell => FMonad cell ()
abort = quit

abort0 :: Cell cell => FMonad cell (CV cell)
abort0 = abort >> return (Val 0)

next :: Cell cell => FMonad cell ()
next = gets ip >>= docol

call :: Cell cell => CV cell -> FMonad cell ()
call (XT name) = abort
call _ = abort

-- Data stack primitives
dpush :: CV cell -> FMonad cell ()
dpush val = modify $ \s -> s { stack = val : stack s }

dpop :: Cell cell => FMonad cell (CV cell)
dpop = gets stack >>= \case
           [] -> abort0
           x:xs -> modify (\s -> s { stack = xs }) >> return x

rpop :: Cell cell => FMonad cell (CV cell)
rpop = gets rstack >>= \case
           [] -> abort0
           x:xs -> modify (\s -> s { rstack = xs }) >> return x

updateState f = do
  result <- StateT f
  case result of
    Left msg -> abortMessage msg
    Right x -> return x

updateStateVal x f = do
  result <- StateT f
  case result of
    Left msg -> abortMessage msg >> return x
    Right y -> return y

newState s = return (Right (), s)

emptyStack = abortWith "empty stack"
abortWith msg s = return (Left msg, s)
abortMessage msg = liftIO (putStrLn msg) >> abort
