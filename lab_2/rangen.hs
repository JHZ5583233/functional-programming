import Data.List (unfoldr)
random :: [Integer]
random = unfoldr (\b -> Just (b , ((25214903917 * b) + 11) `mod` 2^45)) 2773639