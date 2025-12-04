import Data.List (isSubsequenceOf, isInfixOf)
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
neededPiDigits n = stupidSol (toInteger n) (take 1 piDigits)

stupidSol :: Integer -> [Integer] -> Int
stupidSol n xs
    | n < 0 = length xs
    | ns `isInfixOf` xs = stupidSol (n - 1) xs
    | otherwise = stupidSol n (take ((length xs) + 1) piDigits)
    where
      ns = makeList n

makeList :: Integer -> [Integer]
makeList x
  | x < 10 = [x]
  | otherwise = makeList (div x 10) ++ [m]
  where
    m = x `mod` 10
