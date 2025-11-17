collatz :: Integer -> Integer

collatz n
    | n == 1 = 1
    | even n = collatz (div n 2)
    | odd n = collatz ((n * 3) + 1)
