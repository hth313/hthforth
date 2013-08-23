{-|

  Read a Forth source stream file.

-}

module Language.Forth.StreamFile (readSourceFile) where

import Control.Monad.Trans
import Data.Vector.Storable.ByteString (ByteString)
import qualified Data.Vector.Storable.ByteString.Char8 as C

-- | Read given source file.
readSourceFile :: FilePath -> IO ByteString
readSourceFile = C.readFile

