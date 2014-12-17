module Language.Forth.Target (Target(..), alignOffset, TargetKey(..)) where

import Data.Bits
import Util.Endian
import Util.Memory

-- | Target descriptor
data Target cell =  Target
  { bytesPerCell :: Int                     -- ^ native value size
  , bytesPerInstruction :: Int              -- ^ execution token size
  , bytesPerChar :: Int                     -- ^ character size
  , endian :: Endian
  }

alignOffset n target = alignOffset1 n
  where alignOffset1 n | (mask .&. n) /= 0 = alignOffset1 (n + 1)
                       | otherwise = n
        mask = bytesPerCell target - 1

data TargetKey = CortexM | MSP430 deriving (Eq, Ord, Show)
