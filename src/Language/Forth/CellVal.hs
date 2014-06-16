{-|

  Cell values.

-}

module Language.Forth.CellVal (CellVal(..), true, false, isValue, isAddress, isAny,
                               isExecutionToken, isZero) where

import Data.Bits
import Data.Ord
import Data.Map (Map)
import Data.Word
import Language.Forth.Cell
import Data.Vector.Storable.ByteString (ByteString)
import Language.Forth.Address
import Language.Forth.Word


-- | Cell values are what we can put into a data cell.
--   We parameterize over some integer type size (cell).
data CellVal cell a =
    Address (Maybe Addr)    -- ^ An address value
  | Val cell                -- ^ A numeric value
  | XT (ForthWord a)        -- ^ Execution token
  | Text ByteString         -- ^ Some text buffer
  | Bot String

instance Eq cell => Eq (CellVal cell a) where
  a1@Address{} == a2@Address{} = a1 == a2
  v1@Val{} == v2@Val{} = v1 == v2
  t1@Text{} == t2@Text{} = t1 == t2
  _ == _ = False

{-
instance Cell cell => Ord (CellVal cell) where
    compare (Val a) (Val b) = compare a b
    compare (Address a) (Address b) = compare a b
    compare (XT a) (XT b) = comparing name a b
-}

illegalValue = Bot "illegal value"

-- | Make 'CellVal cell' part of Num class. This allows us to use functions such as (+)
--   and many others direct on literals.
instance Cell cell => Num (CellVal cell a) where
    (Val a) + (Val b) = Val (a + b)
    (Address (Just (Addr w off))) + (Val b) = Address (Just (Addr w (off + (fromIntegral b))))
    (Val b) + (Address (Just (Addr w off))) = Address (Just (Addr w (off +  (fromIntegral b))))
    a + b = illegalValue

    (Val a) - (Val b) = Val (a - b)
    (Address (Just (Addr w off))) - (Val b) =
         Address (Just (Addr w  (off + (negate $ fromIntegral b))))
    (Address (Just (Addr w1 off1))) - (Address (Just (Addr w2 off2)))
        | w1 == w2 = Val $ fromIntegral $ off1 - off2
    a - b
        | a == b = Val 0
        | otherwise = illegalValue

    (Val a) * (Val b) = Val (a * b)
    a * b = illegalValue

    abs (Val a) = Val (abs a)
    abs a = illegalValue

    negate (Val a) = Val (negate a)
    negate a = illegalValue

    signum (Val a) = Val (signum a)
    signum a = illegalValue

    fromInteger n = Val (fromInteger n)


-- | Also make 'CellVal cell' part of Bits to allow further operations.
instance Cell cell => Bits (CellVal cell a) where
    (Val a) .&. (Val b) = Val (a .&. b)
    a .&. b = illegalValue
    (Val a) .|. (Val b) = Val (a .|. b)
    a .|. b = illegalValue
    xor (Val a) (Val b) = Val (xor a b)
    xor a b = illegalValue
    complement (Val a) = Val (complement a)
    complement a = illegalValue
    bitSize (Val a) = bitSize a
    isSigned (Val a) = isSigned a
    isSigned _ = False

-- | Boolean truth values.
true, false :: Cell cell => CellVal cell a
true = Val (-1)
false = Val 0

isValue (Val _) = True
isValue _ = False
isAddress Address{} = True
isAddress _ = False
isAny = const True
isExecutionToken XT{} = True
isExecutionToken _ = False

isZero (Val 0) = True
isZero (Address Nothing) = True
isZero _ = False