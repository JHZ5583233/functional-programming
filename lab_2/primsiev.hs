primes::[Integer]
primes = 2 : [(2*x) + 1| x <- [1..], null ([i + j + (2*j*i)| i <- [1.. floor (sqrt (fromIntegral  x))], j <- [i..x], i + j + (2*j*i) <= x, i + j + (2*j*i) == x])]

noNum :: [Integer]
noNum = [i + j + (2*j*i)| i <- [1..], j <- [i..]]