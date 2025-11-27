compose :: [a -> a] -> a -> a
compose = foldl (.) id