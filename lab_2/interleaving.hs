import Data.List (unfoldr)
interleaveRR :: [String] -> String
interleaveRR ss = concat (unfoldr (\x -> if null (onlyHead x) then Nothing else Just (concat(onlyHead x), onlyTail x)) ss)

filterEmpty :: [String] -> [String]
filterEmpty ss = [x | x <- ss, not (null x)]

onlyHead :: [String] -> [String]
onlyHead ss = map (take 1) (filterEmpty ss)

onlyTail :: [String] -> [String]
onlyTail ss = map tail (filterEmpty ss)
