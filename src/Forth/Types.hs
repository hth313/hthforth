{-# LANGUAGE RankNTypes #-}
{-
  This file is part of Planet Pluto Forth.
  Copyright Håkan Thörngren 2011-2013

  Basic types.

-}

module Forth.Types (Lit(..),
                    true, false,
                    isValue, isAddress, isAny, isExecutionToken) where

import Data.Bits
import Data.Map (Map)
import Data.Word
import Forth.Cell
import qualified Data.Vector as V
import Data.Vector.Storable.ByteString (ByteString)
import Forth.Address
import {-# SOURCE #-} Forth.Word

-- | Literals are values that can be stored inside a cell. This is also what goes
--   into a colon definition.
data Lit cell = Address (Maybe Addr) |
                Val cell |
                XT (ForthWord cell) |
                Loc (Maybe (IP cell)) |
                Bot String
                deriving (Eq, Show)


-- | Make 'Lit cell' part of Num class. This allows us to use functions such as (+)
--   and many others direct on literals.
instance Cell cell => Num (Lit cell) where
    (Val a) + (Val b) = Val (a + b)
    (Address (Just (Addr w off))) + (Val b) = Address (Just (Addr w (off + (fromIntegral b))))
    (Val b) + (Address (Just (Addr w off))) = Address (Just (Addr w (off +  (fromIntegral b))))

    a + b = Bot $ show a ++ " " ++ show b ++ " +"
    (Val a) - (Val b) = Val (a - b)
    (Address (Just (Addr w off))) - (Val b) =
         Address (Just (Addr w  (off + (negate $ fromIntegral b))))
    a - b = Bot $ show a ++ " " ++ show b ++ " -"
    (Val a) * (Val b) = Val (a * b)
    a * b = Bot $ show a ++ " " ++ show b ++ " *"
    abs (Val a) = Val (abs a)
    abs a = Bot $ show a ++ " ABS"
    negate (Val a) = Val (negate a)
    negate a = Bot $ show a ++ " NEGATE"
    signum (Val a) = Val (signum a)
    signum a = Bot $ show a ++ " SIGNUM"
    fromInteger n = Val (fromInteger n)

-- | Also make 'Lit cell' part of Bits to allow further operations.
instance Cell cell => Bits (Lit cell) where
    (Val a) .&. (Val b) = Val (a .&. b)
    a .&. b = Bot $ show a ++ " " ++ show b ++ " AND"
    (Val a) .|. (Val b) = Val (a .|. b)
    a .|. b = Bot $ show a ++ " " ++ show b ++ " OR"
    xor (Val a) (Val b) = Val (xor a b)
    xor a b = Bot $ show a ++ " " ++ show b ++ " XOR"
    complement (Val a) = Val (complement a)
    complement a = Bot $ show a ++ " INVERT"
    bitSize (Val a) = bitSize a
    isSigned (Val a) = isSigned a
    isSigned _ = False

-- | Boolean truth values.
true, false :: Cell cell => Lit cell
true = Val (-1)
false = Val 0

isValue (Val _) = True
isValue _ = False
isAddress Address{} = True
isAddress _ = False
isAny = const True
isExecutionToken XT{} = True
isExecutionToken _ = False
