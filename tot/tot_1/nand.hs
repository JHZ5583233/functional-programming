nAnd :: Bool -> Bool -> Bool
nAnd x y
    | (x && y) = False
    | otherwise = True

nAnd2 :: Bool -> Bool -> Bool
nAnd2 x y = not (x && y)