module Util.Memory (Memory(..), newMemory, bufferMemory,
                    read8, read32, write8) where

import Data.Word
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import Data.Bits
import Util.Address
import Util.Endian


-- | Default memory is byte addressable.
data Memory a = Memory {
      readOnly :: Bool,
      baseAddress :: a,
      endAddress :: a,
      endian :: Endian,
      chunk :: ByteString
    }

newMemory start size =
    Memory False start (addAddress start (size - 1)) LittleEndian
           (B.pack $ replicate size 0)

bufferMemory start buf =
    Memory False start (addAddress start (B.length buf - 1)) LittleEndian buf

read8 :: Address a => a -> Memory a -> Word8
read8 adr mem = B.index (chunk mem) (offsetOf adr $ baseAddress mem)

read32 :: Address a => a -> Memory a -> Word32
read32 adr mem = toValue 4 (map fetch [0..3]) mem
    where fetch n = read8 (addAddress adr n) mem

write8 :: Address a => Word8 -> a -> Memory a -> Memory a
write8 val adr mem =
    let i = offsetOf adr $ baseAddress mem
        (p0, p1) = B.splitAt i (chunk mem)
        chunk' = B.concat [p0, B.singleton val, B.tail p1]
    in mem { chunk = chunk' }

toValue n bytes mem =
    sum $ map (uncurry shiftL) (zip (map fromIntegral bytes) (shifts n mem))

shifts n mem = case endian mem of
             LittleEndian -> bits
             BigEndian -> reverse bits
    where bits = take n [0,8..]
