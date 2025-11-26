isCorrectTakuzu :: [String] -> Bool
isCorrectTakuzu = all isCorrect

isCorrect :: String -> Bool
isCorrect s
    | h == '1' = oneEater s
    | h == '0' = zeroEater s
    | otherwise = oneEater s /= zeroEater s
        where
            h = head s

oneEater :: String -> Bool
oneEater s
    | null s = True
    | h == '1' = oneEater xs
    | xh == '1' = zeroEater xs
    | otherwise = zeroEater xs /= zeroEater xxs
    where
        h = head s
        xs = tail s
        xh = head xs
        xxs = tail xs

zeroEater :: String -> Bool
zeroEater s
    | null s = True
    | h == '0' = zeroEater xs
    | xh == '0' = zeroEater xs
    | otherwise = oneEater xs /= oneEater xxs
    where
        h = head s
        xs = tail s
        xh = head xs
        xxs = tail xs