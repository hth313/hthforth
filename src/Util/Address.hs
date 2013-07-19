module Util.Address (Address(..)) where

class Address a where
    -- | Adjust an address by given amount
    addAddress :: a -> Int -> a
    -- | Get offset of memory address with given base address
    offsetOf :: a -> a -> Int
