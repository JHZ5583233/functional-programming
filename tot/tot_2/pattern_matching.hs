patternMatch :: [Integer] -> Integer
patternMatch (a: b : _) = a + b
patternMatch (c : _)  = c
patternMatch [] = 0
