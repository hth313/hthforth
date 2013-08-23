
module Language.Forth.Target (Target(..)) where

import Util.Endian
import Util.Memory


-- | Target descriptor
data Target cell =  Target {
    bytesPerCell :: Int,                     -- ^ native value size
    bytesPerInstruction :: Int,              -- ^ execution token size
    bytesPerChar :: Int,                     -- ^ character size
    endian :: Endian
    }
