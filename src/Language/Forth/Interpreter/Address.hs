module Language.Forth.Interpreter.Address (Addr(..)) where

import Language.Forth.WordId
import Util.Address

-- | All address refer to a data field of some word. In some cases we
--   are pointing to an input buffer, but in those cases we use a special
--   word to hold the buffer, so we still can use the same means.
data Addr = Addr WordId Int deriving (Eq, Ord)

instance Show Addr where
    show (Addr word off) = "#(" ++ show word ++ "," ++ show off ++ ")"

instance Address Addr where
    addAddress (Addr wid disp) n = Addr wid (disp + n)
    offsetOf (Addr _ disp) _ = disp
