import Data.List (sort)
isCorrectTakuzu :: [String] -> Bool
isCorrectTakuzu = all isCorrect

isCorrect :: String -> Bool
isCorrect s = length (unqiue (concat2 [[y| y <- t, possible s y] | x <- s])) == length s
    where
        t = takuzuStrings (length s)

concat2 :: [[a]] -> [a]
concat2 s
    | null s = []
    | otherwise = head s ++ concat2 (tail s)

unqiue :: [a] -> [a]
unqiue s = [x| x <- s, elemNum x s == 1]

possible :: String -> String -> Bool
possible xs ys
    | xs == ys = True
    | head xs == '.' = possible (tail xs) (tail ys)
    | head xs == head ys = possible (tail xs) (tail ys)
    | otherwise = False

elemNum ::forall a. Eq a => a -> [a] -> Integer
elemNum x s = sum [1 | y <- s, y == x]

takuzuStrings :: Int -> [String]
takuzuStrings n = sort [x | x <- oneHelper n ["0", "00"] ++ zeroHelper n ["1", "11"],length x == n, elemNum '1' x == elemNum '0' x]

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