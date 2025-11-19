totient :: Integer -> Integer
totient n
    | n <= 1 = n
    | otherwise = firstLayerOfTotient n 2 n

firstLayerOfTotient :: Integer -> Integer -> Integer -> Integer
firstLayerOfTotient n p r
    | p^2 > n = if n > 1 then r - div r n else r
    | mod n p == 0 = firstLayerOfTotient (reduce n p) (p + 1) (r - div r p)
    | otherwise = firstLayerOfTotient n (p + 1) r

reduce :: Integer -> Integer -> Integer
reduce n p
    | mod n p == 0 = reduce (div n p) p
    | otherwise = n