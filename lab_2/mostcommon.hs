import Data.List (sortBy, minimumBy, maximumBy)
mostCommon :: String -> Char
mostCommon s =  maximum [fst x | x <- zip u c, snd x == m]
    where
        u = unique s []
        c = [count x s| x <- u]
        m = maximum c

unique :: String -> [Char] -> [Char]
unique s cs
    | null s = cs
    | head s `elem` cs = unique (tail s) cs
    | otherwise = unique (tail s) (head s : cs)

count :: Eq a => a -> [a] -> Integer
count l ls
    | null ls = 0
    | head ls == l = 1 + count l (tail ls)
    |otherwise = count l (tail ls)
