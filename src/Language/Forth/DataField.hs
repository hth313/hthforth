{-|

  Data field definition.

-}

module Language.Forth.DataField (DataField(..), newDataField, newBuffer,
                                 textBuffer) where

import Language.Forth.Address
import Language.Forth.CellMemory
import Util.Memory

-- | A data field is the body of a data word. It can either be
--   a cell memory or a byte buffer
data DataField cell a = DataField (CellMemory cell a) | BufferField (Memory Addr)

-- | Allocate a data field of the given size. This is for generic use, writing
--   cells or bytes
newDataField target wid n = DataField $ newCellMemory target n

-- | Allocate a new buffer datafield.  This is not suitable for storing
--   arbitrary cell values, but is suitable and efficient for text buffers.
newBuffer wid n = BufferField $ newMemory (Addr wid 0) n

-- | Allocate a new text buffer with given contents.
textBuffer wid text = BufferField $ bufferMemory (Addr wid 0) text
