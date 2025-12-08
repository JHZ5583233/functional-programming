primes :: [Integer]
primes = 2 : [2*x + 1 | x <- [1..], noCheck2 x]

noCheck :: Integer -> Integer -> Integer -> Bool
noCheck n i j
  | i > n = True
  | j > n = noCheck n (i + 1) (i + 1)
  | y == n = False
  | y > n = noCheck n (i + 1) (i + 1)
  | otherwise = noCheck n i (j + 1)
  where
    y = i + j + 2*i*j

noCheck2 :: Integer -> Bool
noCheck2 n =
  all (not . valid) oddDivs
  where
    m = 2*n + 1
    oddDivs = [d | d <- divisors m, odd d]
    valid a =
      let b = m `div` a
          i = (a - 1) `div` 2
          j = (b - 1) `div` 2
       in a <= b && a * b == m && i >= 1 && j >= i

divisors :: Integer -> [Integer]
divisors m =
  concat [if d*d == m then [d] else [d, m `div` d]
         | d <- [1 .. floor (sqrt (fromIntegral m))]
         , m `mod` d == 0]
