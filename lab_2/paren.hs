import Data.List (foldl)
balancedParens :: String -> Bool
balancedParens ss = reduceParen c == 0
    where
        c = clean ss

clean :: String -> String
clean = filter (`elem` "()")

reduceParen :: String -> Integer
reduceParen = foldl (\a c -> if c == '(' then a + 1 else if a > 0 then a - 1 else a + 9999) 0