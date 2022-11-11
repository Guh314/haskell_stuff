import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors = sum 
                       . nub 
                       . concat 
                       . filterExpand 
                       . expand factors



expand :: [Integer] -> Integer -> [([Integer], Integer)]
expand [] _ = []
expand [0] _ = []
expand (n:ns) limit = ([x| x <- [(minimum (n:ns))..(limit - 1)], x /= 0, x `mod` n == 0], n) : expand ns limit


filterExpand :: [([Integer], Integer)] -> [[Integer]]
filterExpand [] = []
filterExpand [([],_)] = []
filterExpand (n:ns) = fst n : filterExpand ns 
