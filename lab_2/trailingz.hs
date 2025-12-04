import Numeric (showIntAtBase)
import Data.Char (digitToInt, intToDigit)
trailingZeros :: [Integer]
trailingZeros = 1: concat [[0, y] | y <- [z x | x <- [3..], even x]]

intToBits :: Integer -> [Char]
intToBits n = showIntAtBase 2 intToDigit n ""

z :: Integer -> Integer
z x = getlen (intToBits x)

getlen :: [Char] -> Integer
getlen cs
    | '1' `notElem` cs = toInteger (length cs)
    | otherwise = getlen (tail cs)
