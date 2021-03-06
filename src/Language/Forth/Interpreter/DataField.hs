{-|

  Data field definition.

  There are two variants here. Text buffers are stored in a BufferField which
  basically is a ByteString that we manipulate. In other words, raw bits.
  Symbolic cell values are stored into a CellMemory which can handle symbolic
  values.

-}

module Language.Forth.Interpreter.DataField (DataField(..), newDataField, newBuffer,
                                             textBuffer) where

import Language.Forth.Interpreter.Address
import Language.Forth.Interpreter.CellMemory
import Util.Memory

-- | A data field is the body of a data word. It can either be
--   a cell memory or a byte buffer
data DataField a = DataField (CellMemory a) | BufferField (Memory Addr)

-- | Allocate a data field of the given size. This is for generic use, writing
--   cells or bytes
newDataField target wid n = DataField $ newCellMemory target n

-- | Allocate a new buffer datafield.  This is not suitable for storing
--   arbitrary cell values, but is suitable and efficient for text buffers.
newBuffer wid n = BufferField $ newMemory (Addr wid 0) n

-- | Allocate a new text buffer with given contents.
textBuffer wid text = BufferField $ bufferMemory (Addr wid 0) text
