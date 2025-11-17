sumPrimesTo, isqrt:: Integer -> Integer
factors :: Integer -> [Integer]

sumPrimesTo n = sum [x | x <- [2..n], [1] == factors x]

isqrt = floor . sqrt . fromIntegral

factors n = [d | d <- [1..isqrt n], mod n d == 0]