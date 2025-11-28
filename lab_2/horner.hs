evalPoly :: [Integer] -> Integer -> Integer
evalPoly xs x = foldl (\y a -> y * x + a) 0 xs