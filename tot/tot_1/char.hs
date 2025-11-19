import Data.Char (ord)
charToNum :: Char -> Int
charToNum x
    | ord x <= 57 && ord x >= 48 = ord x
    | otherwise = 0