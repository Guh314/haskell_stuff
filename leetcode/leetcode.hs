import Data.List
import Data.Char

-- Problem 1832: Panagram
pangram :: String -> Bool
pangram = (==) 26 . length . nub

pangram' :: String -> Bool
pangram' text = all (`elem` map toUpper text) ['A'..'Z']


-- Problem 1480: Running Sum of 1dArray
runningSum1dArray :: [Int] -> [Int]
runningSum1dArray ns = scanl1 (+) ns


-- Problem 1672: Richest Custommer Wealth
richest :: [[Int]] -> Int
richest = maximum . map sum


-- Problem 1108: Defanging a IP Address
defang :: String -> String
defang = concat
       . map (\x -> if x == '.' then "[.]" else [x])


-- Problem 1431: Max candies
maxCandies :: [Int] -> Int -> [Bool]
maxCandies ns n = map (\x -> x+n >= (maximum ns)) ns


-- Problem 1070: Shuffle the Array
arrayShuffle :: Int -> [Int] -> [Int]
arrayShuffle n ns = concat (transpose (take n ns : drop n ns : []))


-- Problem 1512: Number of Good Pairs
outer :: (a -> b -> c) -> [a] -> [b] -> [[c]]
outer f xs ys = map (\x -> map (f x) ys) xs


numGoodPairs :: Eq a => [a] -> Int
numGoodPairs ns = (sum (map (\x -> if x == True then 1 else 0) (concat (outer (==) ns ns))) - (length ns)) `div` 2


-- Problem 771: Jewels and Stones
jewlsStones :: String -> String -> Int
jewlsStones js = length 
               . filter (/=False)
               . concat 
               . outer (==) js 


-- Problem 1363: How many numbers smaller than the current number
smallerNums :: [Int] -> [Int]
smallerNums lst = map length (map smallerThan lst)
                    where
                        smallerThan x = filter (<x) lst


-- Problem 315: Count of smaller numbers after self.
smallerNumsAfter :: [Int] -> [Int]
smallerNumsAfter []     = []
smallerNumsAfter (x:xs) = sum (map (\y -> if y < x then 1 else 0) xs) : smallerNumsAfter xs
