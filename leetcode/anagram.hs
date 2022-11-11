import Data.Char (toUpper)
import Data.List (group,sort)

import Data.Char (toUpper)
import Data.List (sort)


anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (\x -> (isAnagram x xs) && (map toUpper x /= map toUpper xs))


isAnagram :: String -> String -> Bool
isAnagram x s = sort (map toUpper x) == sort (map toUpper s)



test1 = "master"
test1a = ["stream", "pigeon", "maters"]

test2 = "listen"
test2a = ["enlists", "google", "inlets", "banana"]
