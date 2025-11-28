import Data.List (unfoldr)
interleaveRR :: [String] -> String
interleaveRR ss = concat (unfoldr (\y -> Just (map head y, map tail y) ss))