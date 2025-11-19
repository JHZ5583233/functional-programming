totient :: Integer -> Integer
totient n
    | n == 2 = 1
    | n > 1 = result - div result n
    | otherwise = result
    where
        result = firstLayerOfTotient n 2 n

firstLayerOfTotient :: Integer -> Integer -> Integer -> Integer
firstLayerOfTotient n p r
    | p^2 < n = r
    | mod n p == 0 = firstLayerOfTotient (reduce n p) (p - 1) (r - div r p)
    | otherwise = firstLayerOfTotient n (p + 1) r

reduce :: Integer -> Integer -> Integer
reduce n p
    | mod n p == 0 = reduce (mod n p) p
    | otherwise = n