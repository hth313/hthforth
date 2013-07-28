{-
  This file is part of Planet Pluto Forth.
  Copyright Håkan Thörngren 2011-2013

  Data field definition.

-}

module Forth.DataField (DataField(..), newDataField, newBuffer,
                        textBuffer) where

import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Vector.Storable.ByteString as B
import Forth.Address
import Forth.CellMemory
import Forth.Types
import Forth.WordId
import Util.Memory

-- | A data field is the body of a data word. It can either be
--   a cell memory or a byte buffer
data DataField cell = DataField (CellMemory cell) | BufferField (Memory Addr)

-- | Allocate a data field of the given size. This is for generic use, writing
--   cells or bytes
newDataField target wid n = DataField $ newCellMemory target n

-- | Allocate a new buffer datafield.  This is not suitable for storing
--   arbitrary cell values, but is suitable and efficient for text buffers.
newBuffer wid n = BufferField $ newMemory (Addr wid 0) n

-- | Allocate a new text buffer with given contents.
textBuffer wid text = BufferField $ bufferMemory (Addr wid 0) text
