evalPoly :: [Integer] -> Integer -> Integer
evalPoly = poly

poly :: [Integer] -> Integer -> Integer
poly xs i
    | null xs = 0
    | otherwise = head xs * (i ^ (length xs - 1)) + poly (tail xs) i