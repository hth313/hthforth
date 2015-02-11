{-

  Symbolic expressions.

-}

module Translator.Expression (Expr(..), reduceExpr, reduce,
                              rangeCheck, signedRangeCheck, anyRangeCheck, rangeCheckRange,
                              rcInRange, rcMasked, rcRange, bottomp,
                              multiply, divide, modulo, shiftLeft, shiftRight,
                              gte, lte, gt, lt, eq, neq, lognot,
                              locationCounter,
                              ebitcomplement, eadd, esub, ebitand, ebitor, ebitxor, RangeInfo(..)) where

import Data.Bits
import Data.Char
import Data.Int
import Data.Maybe
import Data.Word
import qualified Data.Ix as Ix
import Control.Exception
import qualified Data.ByteString.Char8 as B
import Translator.Symbol

type BaseVal = Int64

-- Expression objects
type Operator1 = (String, (BaseVal -> BaseVal))
type Operator2 = (String, (BaseVal -> BaseVal -> BaseVal))
data RangeInfo = RangeInfo (BaseVal, BaseVal) BaseVal -- range, bitmask
data Expr = Value BaseVal | Identifier Symbol |
            Binary Operator2 Expr Expr | Unary Operator1 Expr |
            Select Expr [Expr] | Bottom { bottomText :: String } |
            QMark Expr Expr Expr | Fetch Expr |
            UnknownExpr -- used for unknown values in symbol tree

-- Show expression, mostly intended for interactive use at the moment
instance Show Expr where
    show (Value n) = show n
    show (Identifier id) = B.unpack id
    show (Binary (op,_) e1 e2) = show e1 ++ op ++ show e2
    show (Unary (op,_) e) = op ++ show e
    show (Select n elts) = "select " ++ show n ++ " of " ++ show elts
    show (QMark t e1 e2) = "(" ++ show t ++ " ? " ++ show e1 ++ " : " ++ show e2 ++ ")"
    show (Bottom err) = "_|_"
    show UnknownExpr = "UnknownExpr"
    show (Fetch e) = "Fetch(" ++ show e ++ ")"

instance Eq Expr where
    Value e1 == Value e2 = e1 == e2
    e1 == e2 = (show e1) == (show e2)

instance Ord Expr where
    Value e1 <= Value e2 = e1 <= e2
    e1 <= e2 = (show e1) <= (show e2)

instance Num Expr where
    (+) = eadd
    (-) = esub
    (*) = multiply
    abs (Value n) = Value (abs n)
    abs e = Unary ("abs", abs) e
    signum (Value n) = Value (signum n)
    signum e = Unary ("signum", signum) e
    fromInteger n = Value (fromIntegral n)

instance Bits Expr where
    (.&.) = ebitand
    (.|.) = ebitor
    xor = ebitxor
    complement = ebitcomplement
    bitSize _ = case Value 0 of
                  Value x -> bitSize x
    isSigned e = True
    bit n = Value (1 `shiftL` n)
    testBit (Value x) n = (x .&. (1 `shiftL` n)) /= 0
    popCount (Value x) = popCount x

locationCounter = Identifier (B.pack "*")

-- Traverse an expression and apply a reducing function to each sub-expression.
reduceExpr f e = rex e
    where rex (Binary op e1 e2) = f (Binary op (rex e1) (rex e2))
          rex (Unary op e) = f (Unary op (rex e))
          rex (Select n elts) = f (Select (rex n) (map rex elts))
          rex (QMark t e1 e2) = f (QMark (rex t) (rex e1) (rex e2))
          rex (Fetch e) = f (Fetch (rex e))
          rex e = f e

-- Expression reducer that will attempt to reduce a given sub-expression.
-- To be used together with reduceExpr
reduce (Binary (_, f) (Value n1) (Value n2)) = Value (f n1 n2)
reduce (Unary (_, f) (Value n)) = Value (f n)
reduce (Binary ("+", _) (Value 0) e) = e
reduce (Binary ("+", _) e (Value 0)) = e
reduce (Binary add@("+", _) (Binary ("+", _) e (Value n1)) (Value n2)) = Binary add e (Value $ n1 + n2)
-- Try to get numeric offset on the right hand side
reduce (Binary add@("+", _) (Binary ("+", _) (Value n1) e) (Value n2)) = Binary add e (Value $ n1 + n2)
reduce (Binary add@("+", _) e1@Value{} e2) = Binary add e2 e1
reduce (Binary ("-", _) (Value 0) e) = chs e
reduce (Binary ("-", _) e (Value 0)) = e
reduce (Select (Value n) elts) = elts !! fromIntegral n
reduce (QMark (Value 0) e1 e2) = e2
reduce (QMark (Value _) e1 e2) = e1
reduce (Binary _ bot@(Bottom _) _) = bot
reduce (Binary _ _ bot@(Bottom _)) = bot
reduce (Unary _ bot@(Bottom _)) = bot
reduce (Select bot@(Bottom _) _) = bot
reduce (Select _ choices)
    | any bottomp choices = head $ filter bottomp choices
reduce (QMark bot@(Bottom _) e1 e2) = bot
reduce (QMark _ bot@(Bottom _) _) = bot
reduce (QMark _ _ bot@(Bottom _)) = bot
reduce (Fetch bot@(Bottom _)) = bot
reduce e = e

bottomp (Bottom _) = True
bottomp _ = False

rcRange (RangeInfo r _) = r
rcInRange ri = Ix.inRange (rcRange ri)
rcMasked (RangeInfo _ mask) | mask /= 0 = (mask .&.)
                            | otherwise = id

rangeCheck hi = RangeInfo (0, hi) 0
rangeCheckRange range = RangeInfo range 0

signedRangeCheck bits = RangeInfo range mask
    where mask = (bit bits) - 1
          val = mask `shiftR` 1
          range = (-val - 1, val)

anyRangeCheck bits = RangeInfo range mask
    where mask = (bit bits) - 1
          val = mask `shiftR` 1
          range = (-val - 1, mask)

-- Define expression operators
operator op fn (Value n1) (Value n2) = Value (fn n1 n2)
operator op fn e1 e2 = Binary (op, fn) e1 e2

eadd = operator "+" (+)
esub = operator "-" (-)
multiply = operator "*" (*)
divide = operator "/" div
modulo = operator "%" mod
shiftLeft = operator "<<" (shiftFun shiftL)
shiftRight = operator ">>" (shiftFun shiftR)
ebitand = operator "&" (.&.)
ebitor = operator "|" (.|.)
ebitxor = operator "^" xor
ebitcomplement (Value n) = Value (complement n)
ebitcomplement e = Unary ("~", complement) e
gt = operator ">" (bool2Num (>))
lt = operator "<" (bool2Num (<))
gte = operator ">=" (bool2Num (>=))
lte = operator "<=" (bool2Num (<=))
eq = operator "==" (bool2Num (==))
neq = operator "!=" (bool2Num (/=))
lognot (Value n) = Value (notn n)
lognot e = Unary ("!", notn) e
chs (Value n) = Value (- n)
chs e = Unary ("-", negate) e
bool2Num f x y = case f x y of
                   False -> 0
                   True -> 1
notn n = (if n /= 0 then 0 else 1)

shiftFun f val n = f val (fromIntegral n)
