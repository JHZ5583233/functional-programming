rangeProduct :: Integer -> Integer -> Integer
rangeProduct n m
    | n < m = 0
    | otherwise = product [m..n]

fac :: Integer -> Integer
fac = rangeProduct 1

twoPow :: Integer -> Integer
twoPow n
    | odd n = 2 * twoPow (n - 1)
    | even n = twoPow (div n 2) ^ 2