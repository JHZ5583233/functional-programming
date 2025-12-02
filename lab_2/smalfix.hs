fixpoint :: (Integer -> Integer) -> Integer
fixpoint f = hell f 0

hell :: (Integer -> Integer) -> Integer -> Integer
hell f i
    | r == i = i
    | otherwise = hell f (i + 1)
    where
        r = f i