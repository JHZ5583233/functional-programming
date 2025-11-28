increasing :: [Integer] -> [Integer]
increasing xs = map snd (filter (uncurry (<)) (zip xs (tail xs)))