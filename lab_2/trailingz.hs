trailingZeros :: [Integer]
trailingZeros = concat [[0, y] | y <- [z (2*x) | x <- [2..]]]

z :: Integer ->  Integer
z n
    | 