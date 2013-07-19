
module Forth.Target (Target(..)) where

import Util.Memory


-- | Target descriptor
data Target cell =  Target {
    bytesPerCell :: Int,                     -- ^ native value size
    bytesPerInstruction :: Int,              -- ^ execution token size
    bytesPerChar :: Int,                     -- ^ character size
    readCell :: Addr -> Memory Addr -> cell,
    writeCell :: cell -> Addr -> Memory Addr -> Memory Addr
    }
