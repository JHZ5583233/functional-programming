properLength :: [a] -> Integer
properLength xs = toInteger (length xs)

fiboUpTo :: Integer -> [Integer] -> Integer -> Integer -> [Integer]
fiboUpTo n xs a b
    | a > n = xs
    | otherwise = fiboUpTo n (a : xs) b (a + b)

greedyList :: [Integer] -> Integer -> [[Integer]]
greedyList f n
    | length f == 1 = [[last f]]
    | otherwise = [fn : xs |
                   let fn = last f,
                   xs <- greedyList (init f) n,
                   sum (fn : xs) <= n] ++
                   [[last f]] ++
                   greedyList (init f) n

numberOfFiboSums :: Integer -> Integer
numberOfFiboSums n
    | n == 0 = 1
    |otherwise = properLength [x |
                               x <- greedyList (fiboUpTo n [] 1 2) n,
                               sum x == n]