import Data.Char

test1 = "Portable Network Graphics"
test2 = "Ruby on Rails"
test3 = "HyperText Markup Language"
test4 = "Complementary metal-oxide semiconductor"


import Data.Char

abbreviate :: String -> String
abbreviate xs
    | any (\x -> (x == '-') || (x == '_')) xs = abbreviate (map (\x -> if x == '-' || x == '_' then ' ' else x) xs)
    | any (> 2) (verify xs) = map toUpper (map (\x -> head x) (words xs))
    | any (> 1) (verify xs) = filter isUpper xs
    | any (\x -> isUpper (head x)) (words xs) = map toUpper (map (\x -> head x) (words xs))

verify :: String -> [Int]
verify = map length
       . map (filter isUpper) 
       . words
