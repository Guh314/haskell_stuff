import Data.Char (toLower, isAlphaNum)
import Data.List (transpose)

test :: String
test = "If man was meant to stay on the ground, god would have given us roots."


encode :: String -> String
encode xs =
    | xs == "" = ""
    | length xs > 0 = unwords (transpose (table (normalize xs) (acert (normalize xs))))


normalize :: String -> String
normalize = map toLower . filter isAlphaNum


possibleCrypt :: Int -> [(Int, Int)]
possibleCrypt a = [(x,y) | x <- [1..a], y <- [1..a], x >= y, (x - y) <= 1]


acert :: String -> (Int,Int)
acert xs = head (dropWhile (\(x,y) -> (x * y) < (length xs)) (possibleCrypt (length xs)))


table :: String -> (Int,Int) -> [String]
table xs (x,y)
    | length xs == x * y = split xs (x,y)
    | otherwise = table (xs ++ " ") (x,y)
    where
        split xs (x,y)
            | xs == [] = []
            | otherwise = take x xs : split (drop x xs) (x,y)
