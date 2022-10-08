--Higher-order Functions
--

-- Functions that receives another function as an argument or returns a function

twice :: (a -> a) -> a -> a
twice f x = f (f x)


-- List Processing
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]


recMap :: (a -> b) -> [a] -> [b]
recMap _ [] = []
recMap f (x:xs) = f x : recMap f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x|x <- xs, p x]


recFilter :: (a -> Bool) -> [a] -> [a]
recFilter p [] = []
recFilter p (x:xs) | p x = x : recFilter p xs
                   | otherwise = recFilter p xs


sumsqreven :: [Int] -> Int
sumsqreven ns = sum (map (^2) (filter even ns))


all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f (x:xs) | f x = all' f xs
              | otherwise = False


any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f (x:xs) | f x = True
              | otherwise = any' f xs


takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) | f x = x : takeWhile' f xs
                    | otherwise = []


dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) | f x = dropWhile' f xs
                    | otherwise = x:xs

-- Foldr function

-- Basic Template of Foldr
{-
 - f []     = v
 - f (x:xs) = x # f xs
-}

regularSum :: Num a => [a] -> a
regularSum []     = 0
regularSum (x:xs) = 1 + regularSum xs

sum' :: Num a => [a] -> a
sum' = foldr (+) 0

product' :: Num a => [a] -> a
product' = foldr (*) 1

or' :: [Bool] -> Bool
or' = foldr (||) False

and' :: [Bool] -> Bool
and' = foldr (&&) True


explicitSum :: Num a => [a] -> a
explicitSum xs = foldr (+) 0 xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v [] = v
foldr' f v (x:xs) = f x (foldr' f v xs)


length' :: [a] -> Int
length' [] = 0
length' (_:xs) = 1 + length' xs


foldrLength :: [a] -> Int
foldrLength = foldr (\_ n -> 1+n) 0


recReverse :: [a] -> [a]
recReverse []     = []
recReverse (x:xs) = recReverse xs ++ [x]


snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]


redReverse :: [a] -> [a]
redReverse []     = []
redReverse (x:xs) = snoc x (redReverse xs)


foldrReverse :: [a] -> [a]
foldrReverse = foldr snoc []


lambdaFoldrReverse :: [a] -> [a]
lambdaFoldrReverse xs = foldr (\x xs -> xs ++ [x]) [] xs


--Foldl Functions
sum_ex :: Num a => [a] -> a
sum_ex = sum' 0
        where
            sum' v [] = v
            sum' v (x:xs) = sum' (v+x) xs


foldl_sum :: Num a => [a] -> a
foldl_sum = foldl (+) 0


foldl_prod :: Num a => [a] -> a
foldl_prod = foldl (*) 1


foldl_or :: [Bool] -> Bool
foldl_or = foldl (||) False


foldl_and :: [Bool] -> Bool
foldl_and = foldl (&&) True


foldl_length :: [a] -> Int
foldl_length = foldl (\n _ -> n+1) 0


foldl_reverse :: [a] -> [a]
foldl_reverse = foldl (\xs x -> x:xs) []


rec_foldl :: (a -> b -> a) -> a -> [b] -> a
rec_foldl f v []     = v
rec_foldl f v (x:xs) = rec_foldl f (f v x) xs


--Composition operator
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- f . g = \x -> f (g x)

composed_odd :: Int -> Bool
composed_odd = not . even

composed_twice :: (a -> a) -> a -> a
composed_twice f = f . f

composed_sumsqreven :: [Int] -> Int
composed_sumsqreven = sum . map (^2) . filter even

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id


--Exercises

--1
-- [f x | x <- xs, p x]
-- comp = map f . filter p

--2
--2.a
allQ :: (a -> Bool) -> [a] -> Bool
allQ p = and . map p

--2.b
anyQ :: (a -> Bool) -> [a] -> Bool
anyQ p = or . map p

--2.c
takeWhileQ :: (a -> Bool) -> [a] -> [a]
takeWhileQ _ []                 = []
takeWhileQ p (x:xs) | p x       = x : takeWhileQ p xs
                    | otherwise = []

--2.d
dropWhileQ :: (a -> Bool) -> [a] -> [a]
dropWhileQ _ []                 = []
dropWhileQ p (x:xs) | p x       = dropWhileQ p xs
                    | otherwise = (x:xs)

--3
-- map f = foldr (\x xs -> f x : xs) []
-- filter p = foldr (\x xs -> if p x then x:xs else xs) []

--4
dec2int :: [Int] -> Int
dec2int = foldl (\x y -> x * 10 + y) 0

--5
curry' :: ((x,y) -> z) -> (x -> y -> z)
curry' f = \x y -> f (x,y)

uncurry' :: (x -> y -> z) -> ((x,y) -> z)
uncurry' f = \(x,y) -> f x y

--6
unfold :: (t -> Bool) -> (t -> a) -> (t -> t) -> t -> [a]
unfold p h t x | p x       = []
               | otherwise = h x : unfold p h t (t x)

int2bin :: Integer -> [Integer]
int2bin = unfold (==0) (`mod`2) (`div`2)

mapU :: (a -> b) -> [a] -> [b]
mapU f = unfold null (f . head) tail

chop8 :: [Integer] -> [[Integer]]
chop8 = unfold null (take 8) (drop 8)

iterate :: (a -> a) -> a -> [a]
iterate = unfold (const False) id


altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f0 f1 (x:xs) = f0 x : altMap f1 f0 xs
