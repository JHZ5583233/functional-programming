isNthRootOfUnity :: Integer -> Integer -> Integer -> Bool
isNthRootOfUnity x n m
    | powMod x n m 1 == 1 = True
    | otherwise = False

powMod :: Integer -> Integer -> Integer -> Integer -> Integer
powMod x n m r
    | n == 0 = r
    | even n = powMod (mod (x^2) m) (div n 2) m r
    | odd n = powMod x (n - 1) m (mod (r * x) m)
