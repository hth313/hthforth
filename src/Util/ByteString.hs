{-# LANGUAGE ForeignFunctionInterface #-}
-- | Extend vector-bytestring to allow for O(1) updates in a very
--   unsafe way. Use with care...
module Util.ByteString (unsafeUpdate, memmove) where

import Control.Monad (void)
import Foreign (Ptr, Word8)
import Foreign.C.Types (CSize(..))
import Foreign.ForeignPtr (withForeignPtr)
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable as VS


foreign import ccall unsafe "static bytestring.h bytestring_update" c_update
    :: Ptr Word8 -> CSize -> Word8 -> IO ()

-- | Modify a single element inside the ByteString in a destructive manner.
--   Using this requires we take care not to share this bytestring.
unsafeUpdate :: Word8 -> ByteString -> Int -> IO ()
unsafeUpdate x v i = withForeignPtr fp update1 where
    (fp, l) = VS.unsafeToForeignPtr0 v
    update1 p | i < l = c_update p (fromIntegral i) x
              | otherwise = return ()
{-# INLINE unsafeUpdate #-}

foreign import ccall unsafe "string.h memmove" c_memmove
    :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)

memmove :: Ptr Word8 -> Ptr Word8 -> Int -> IO ()
memmove p q s = void $ c_memmove p q (fromIntegral s)
