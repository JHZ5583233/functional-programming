chebyshev :: Integer -> Integer

chebyshev n = f n 0 2
    where
        f 0 a b = a
        f i a b = f (i - 1) b ((4 * b) - a)