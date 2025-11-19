countHappyNumbers :: Integer -> Integer -> Int
countHappyNumbers a b = length [x | x <- [a..b], isHappy x []]

isHappy :: Integer -> [Integer] -> Bool
isHappy x hs
    | x `elem` hs = False
    | x == 1 = True
    | otherwise = isHappy next (x: hs)
    where
        next = sum [p^2 | p <- toDigits x]

toDigits :: Integer -> [Integer]
toDigits n
    | n < 10 = [n]
    | otherwise = mod n 10 : toDigits (div n 10)