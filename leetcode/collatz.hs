-- Solving Collatz

collatz n
    | n < 1 = 0
    | n == 1 = 0
    | even n = 1 + collatz (div n 2)
    | odd n = 1 + collatz ((3*n)+1)
    | otherwise = 0
