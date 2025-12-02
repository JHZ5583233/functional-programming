import Data.List (group, sort, maximumBy )
mostCommon :: String -> Char
mostCommon s =  maximum [fst y | y <- zip l c, snd y == m]
    where
        l = unique s
        c = [length (filter (== x) s) | x <- unique s]
        m = maximum c

unique :: String -> [Char]
unique s = removeRuns (sort s)

removeRuns :: String -> String
removeRuns ss = map head (group ss)