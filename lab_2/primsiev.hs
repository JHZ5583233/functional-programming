primes::[Integer]
primes = 2 : [(2*x) + 1| x <- [1..], noList x 1 1]

noList :: Integer -> Integer -> Integer -> Bool
noList n i j
  | (i+j+(2*i*j)) > n = True
  | (i+j+(2*i*j)) == n = False
  | otherwise = noList n (i + 1) j && noList n (i + 1) (j + 1) && noList n i (j + 1)

