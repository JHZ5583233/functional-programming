properLength :: [a] -> Integer
properLength xs = toInteger (length xs)

fiboUpTo :: Integer -> [Integer] -> Integer -> Integer -> [Integer]
fiboUpTo n xs a b
    | a > n = xs
    | otherwise = fiboUpTo n (a : xs) b (a + b)

greedyList :: [Integer] -> [[Integer]] -> Integer -> [[Integer]]
greedyList f sss n
    | length f == 1 = [[last f]]
    | otherwise = [x | x <- fn : xs,
                    xs <- greedyList (init f) sss n, fn <- last f,
                    sum (fn : xs) <= n]

numberOfFiboSums :: Integer -> Integer
numberOfFiboSums n = properLength [xs | xs <- fiboUpTo n [] 1 2]