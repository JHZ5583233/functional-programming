primes :: [Integer]
primes = 2 : [2*x + 1 | x <- [1..], noCheck x 1 1]

noCheck :: Integer -> Integer -> Integer -> Bool
noCheck n i j
  | i > n = True
  | j > n = noCheck n (i + 1) (i + 1)
  | y == n = False
  | y > n = noCheck n (i + 1) (i + 1)
  | otherwise = noCheck n i (j + 1)
  where
    y = i + j + 2*i*j