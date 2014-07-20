module Util.Address (Address(..)) where

class (Eq a, Ord a) => Address a where
    -- | Adjust an address by given amount
    addAddress :: a -> Int -> a
    -- | Set the offset of the address
    setOffset :: a -> Int -> a
    -- | Get offset of memory address with given base address
    offsetOf :: a -> a -> Int
