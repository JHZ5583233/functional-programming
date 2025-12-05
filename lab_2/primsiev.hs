primes::[Integer]
primes = 2 : [(2*x) + 1| x <- [1..], x `notElem` [i + j + (2*j*i)| i <- [1..x], j <- [i..x]]]

noList :: Integer -> [Integer]
noList x
    | x == 1 = [i + x + (2*x*i)| i <- [1..x]]
    | otherwise = ([i + x + (2*x*i)| i <- [1..x]]) ++ lastl
        where
            lastl = noList (x - 1)
