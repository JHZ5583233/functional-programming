import Data.List (sort)

takuzuStrings :: Int -> [String]
takuzuStrings n = sort [x | x <- oneHelper n ["0", "00"] ++ zeroHelper n ["1", "11"], length x == n]

oneHelper :: Int -> [String] -> [String]
oneHelper n xs
    | new == xs = xs
    | otherwise = zeroHelper n new
    where
        new = [x ++ h | x <- xs, h <- ["1", "11"], length (h ++ x) <= n] ++ [p | p <- xs, length p == n]

zeroHelper :: Int -> [String] -> [String]
zeroHelper n xs
    | new == xs = xs
    | otherwise = oneHelper n new
    where
        new = [x ++ h | x <- xs, h <- ["0", "00"], length (h ++ x) <= n] ++ [p | p <- xs, length p == n]