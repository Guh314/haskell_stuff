import Data.Char

type Bit = Int


--bit2int :: [Bit] -> Int
--bit2int bits = sum [w*b | (w,b) <- zip weights bits]
--            where weights = iterate (*2) 1


bit2int :: [Bit] -> Int
bit2int = foldr (\x y -> x + 2*y) 0

int2bit :: Int -> [Bit]
int2bit 0 = []
int2bit n = n `mod` 2 : int2bit (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . map (make8 . int2bit . ord)

chop8 :: [Bit] -> [[Bit]]
chop8 []   = []
chop8 bits = take 8 bits : chop8 (drop 8 bits)

decode :: [Bit] -> String
decode = map (chr . bit2int) . chop8

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id
