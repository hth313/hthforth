{-# LANGUAGE RankNTypes #-}
{-
  This file is part of CalcForth.
  Copyright Håkan Thörngren 2011

  Basic types.

-}

module Forth.Types (DataField(..), Lit(..), WordId(..), Addr(..),
                    true, false,
                    isValue, isAddress, isAny, isExecutionToken) where

import Data.Bits
import Data.Map (Map)
import Data.Word
import qualified Data.Vector as V
import Data.Vector.Storable.ByteString (ByteString)
import {-# SOURCE #-} Forth.Word
import Forth.Cell

-- | Literals are values that can be stored inside a cell.
data Lit cell = Address (Maybe Addr) |
                Val cell |
                ExecutionToken (ForthWord cell) |
                Loc (IP cell) |
--                InputSource ByteString |
                Bot String |
                UndefinedValue
                deriving (Eq, Show)

-- | Unique identifier for words.
type WordId = Int

-- | All address refer to a data field of some word. In some cases we
--   are pointing to an input buffer, but in those cases we use a special
--   word to hold the buffer, so we still can use the same means.
data Addr = Addr WordId Int deriving Eq

instance Show Addr where
    show (Addr word off) = "#(" ++ show word ++ "," ++ show off ++ ")"

{-
showAddress kind key offset = "<" ++ show key ++ "::" ++ kind ++ " " ++ show offset ++ ">"
-}

-- DataObject is what is written to memory, it will either be a cell or char value.
-- Thus, it introduces a wrapper for these alternatives.
--data DataObject cell = Cell (Lit cell) | Byte Word8 | Undefined

-- A data field is the body of a data word
newtype DataField = DataField ByteString

{-
data DataField cell = DataField { dataSize :: cell,
                                  writable :: Bool,
                                  objects :: Map cell (DataObject cell) }
-}

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
isExecutionToken (ExecutionToken _) = True
isExecutionToken _ = False
