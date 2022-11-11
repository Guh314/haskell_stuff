import Data.Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs 
  | length xs == 0 = Right base
  | length (translate xs) == 0 = Left xs
  | length xs == length (translate xs) = Right (fromList (mapping (translate xs)))
  | otherwise = Left xs
  where
    base = (fromList [(A, 0), (C, 0), (G, 0), (T, 0)])


translate :: String -> [Nucleotide]
translate "" = []
translate xs
    | head xs == 'A' = A : pass xs 
    | head xs == 'C' = C : pass xs  
    | head xs == 'G' = G : pass xs  
    | head xs == 'T' = T : pass xs 
    | otherwise      = []
    where
        pass xs = translate (tail xs)

countN :: Nucleotide -> [Nucleotide] -> Int
countN _ [] = 1
countN n xs
    | head xs == n = 1 + countN n (tail xs)
    | otherwise = countN n (tail xs)

mapping :: [Nucleotide] -> [(Nucleotide, Int)]
mapping [] = []
mapping (x:xs) = (x, countN x xs) : mapping (filter (/= x) xs)
