productEvenOdd :: [Integer] -> (Integer, Integer)
productEvenOdd xs = (product [x | x <- xs, even x], product [x | x <- xs, odd x])