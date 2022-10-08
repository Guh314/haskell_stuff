-- Normal Function

fac :: Int -> Int
fac n = product [1..n]



-- Recursive functions
-- Basic Recursion
fac' :: Int -> Int
fac' 0 = 1
fac' n | n > 0 = n * fac' (n-1)


-- Recursion on lists
product' :: Num a => [a] -> a
product' [] = 1
product' (n:ns) = n * product ns


lenght' :: Num a => [a] -> Int
lenght' [] = 0
lenght' (_:xs) = 1 + lenght' xs


reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]


insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x : y : ys
                 | otherwise = y : insert x ys


isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)


-- Recursion with multiple arguments
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = [] 
zip' _ [] = []
zip' (x:xs) (y:ys) = [(x, y)] ++ zip' xs ys


drop' :: Int -> [a] -> [a]
drop' 0 xs = xs
drop' _ [] = []
drop' n (_:xs) = drop' (n-1) xs


-- Multiple Recursion
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where
                   smaller = [a|a<-xs, a <= x]
                   larger  = [b|b<-xs, b >  x]


-- Mutual Recursion
even' :: Int -> Bool
even' 0 = True
even' n = odd' (n-1)

odd' :: Int -> Bool
odd' 0 = False
odd' n = even' (n-1)


evens :: [a] -> [a]
evens [] = []
evens (x:xs) = x : odds xs

odds :: [a] -> [a]
odds [] = []
odds (_:xs) = evens xs



-- Other recursions
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

{-
(^) :: Int -> Int -> Int
m ^ 0 = 1
m ^ n = m * (m ^ (n-1))
-}


init' :: [a] -> [a]
init' [_] = []
init' (x:xs) = x : init' xs


euclid :: Int -> Int -> Int
euclid n m | n < m = euclid n (m-n)
           | n > m = euclid (n-m) m
           | n == m = n



and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) | x == False = False
            | otherwise = and' xs


concat' :: [[a]] -> [a]
concat' [[]] = []
concat' (x:xs) = x ++ concat xs


replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate' (n-1) x


--getNth == (!!)
getNth :: [a] -> Int -> a
getNth (x:xs) 0 = x
getNth (_:xs) n = getNth xs (n-1)


elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' n (x:xs) | n == x = True
               | otherwise = elem' n xs



merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys


msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort lower_halve) (msort upper_halve)
          where
            halve_res = halve xs
            lower_halve = fst halve_res
            upper_halve = snd halve_res


halve :: [a] -> ([a], [a])
halve [] = ([],[])
halve [x] = ([x], [])
halve (x:xs) = (lower_half, upper_half)
              where
                lower_half = take (div (length (x:xs)) 2) (x:xs)
                upper_half = drop (div (length (x:xs)) 2) (x:xs)


sum' :: Num a => [a] -> a
sum' = foldr (+) 0


take' :: Int -> [a] -> [a]
take' 0 xs = []
take' _ [] = []
take' n (x:xs) = [x] ++ take' (n-1) xs


last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs
