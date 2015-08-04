module Util.Memory (Memory(..), newMemory, bufferMemory, validAddress,
                    read8, read32, write8, blockMove, blockMoveText) where

import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import Control.Applicative ((<$>))
import Data.Word
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString as B
import Data.Vector.Storable.ByteString.Internal
import Data.Bits
import Util.Address
import Util.ByteString
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

validAddress :: Address a => a -> Memory a -> Bool
validAddress adr mem = adr >= baseAddress mem && adr <= endAddress mem

read8 :: Address a => a -> Memory a -> Maybe Word8
read8 adr mem
  | validAddress adr mem = Just $ B.index (chunk mem) (offsetOf adr $ baseAddress mem)
  | otherwise = Nothing

read32 :: Address a => a -> Memory a -> Maybe Word32
read32 adr mem = toValue 4 mem <$> (sequence $ map fetch [0..3])
    where fetch n = read8 (addAddress adr n) mem

write8 :: Address a => Word8 -> a -> Memory a -> IO ()
write8 val adr mem =
    let i = offsetOf adr $ baseAddress mem
    in unsafeUpdate val (chunk mem) i

toValue n mem bytes =
    sum $ map (uncurry shiftL) (zip (map fromIntegral bytes) (shifts n mem))

shifts n mem = case endian mem of
             LittleEndian -> bits
             BigEndian -> reverse bits
    where bits = take n [0,8..]

blockMove :: Address a => Int -> a -> Memory a -> a -> Memory a -> IO ()
blockMove count adrFrom memFrom adrTo memTo =
    let (fpFrom, offsetFrom) = decode adrFrom memFrom
        (fpTo, offsetTo) = decode adrTo memTo
        decode adr mem =
            let (fp, offset, len) = toForeignPtr $ chunk mem
            in (fp, offset + offsetOf adr (baseAddress mem))
    in withForeignPtr fpFrom  $ \from ->
         withForeignPtr fpTo $ \to ->
             memmove (to `plusPtr` offsetTo) (from `plusPtr` offsetFrom) (fromIntegral count)

blockMoveText text adrTo memTo =
  let n = B.length text
      -- Create an address based on the destination address. We are providing
      -- another memory block, which makes it work even though the address
      -- is partly a lie (the part we lie about is not used at this level).
      adrFrom = setOffset adrTo 0
      memFrom = bufferMemory adrFrom text
  in blockMove n (baseAddress memFrom) memFrom adrTo memTo
