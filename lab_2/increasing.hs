increasing :: [Integer] -> [Integer]
increasing xs
    | length xs <= 1 = []
    | head xs < head (tail xs) =  head (tail xs) : increasing (tail xs)
    | otherwise = increasing (tail xs)