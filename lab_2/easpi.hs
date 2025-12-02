-- Infinite list of decimal digits of pi: 3,1,4,1,5,9,...
piDigits :: [Integer]
piDigits = digits 1 0 1 1 3 3
  where
    digits q r t k n l
      | 4*q + r - t < n*t =
          -- We can safely emit digit n
          n : digits (10*q) (10*(r - n*t)) t k n' l
      | otherwise =
          -- Need to refine the approximation first
          digits (q*k) ((2*q + r)*l) (t*l) (k+1) n'' (l+2)
      where
        n'  = (10*(3*q + r)) `div` t - 10*n
        n'' = (q*(7*k + 2) + r*l) `div` (t*l)

neededPiDigits :: Int -> Int
neededPiDigits n = stupidSol n 1

stupidSol :: Int -> Int -> Int
stupidSol n x
    | all  (`elem` take x piDigits) [0..toInteger n] = x
    | otherwise = stupidSol n (x + 1)

subList :: [Integer] -> [Integer] -> Bool
subList xs ys
    | null xs = True
    | null ys = False
    | head xs == head ys = subList (tail xs) (tail ys)
    | otherwise = subList xs (tail ys)