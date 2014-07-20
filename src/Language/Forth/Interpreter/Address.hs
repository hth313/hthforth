module Language.Forth.Interpreter.Address (Addr(..), module Util.Address) where

import Language.Forth.Word
import Util.Address

-- | All address refer to a data field of some word. In some cases we
--   are pointing to an input buffer, but in those cases we use a special
--   word to hold the buffer, so we still can use the same means.
data Addr = Addr WordId Int deriving (Eq, Ord)

instance Show Addr where
    show (Addr word off) = "#(" ++ show word ++ "," ++ show off ++ ")"

instance Address Addr where
    addAddress (Addr wid disp) n = Addr wid (disp + n)
    setOffset (Addr wid _) n = Addr wid n
    offsetOf (Addr _ disp1) (Addr _ disp2) = disp1 + disp2
