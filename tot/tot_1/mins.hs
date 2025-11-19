min2 :: Int -> Int -> Int
min2 x y
    | x > y = x
    | otherwise = y

minThree :: Int -> Int -> Int -> Int
minThree x y z = min2 x (min2 y z)