data Clock = Full String String
    deriving (Eq)



c1 :: Clock
c1 = Full "14" "50"


toString :: Clock -> String
toString c =
    case c of
        Full h m -> h ++ ":" ++ m


fromHourMin :: Int -> Int -> Clock
fromHourMin h m 
    | m >= 60 = correctMin h m
    | m < 0 = negativeMin h m
    | h == 24 = fromHourMin 0 m
    | h < 0 = negativeHour h m
    | h > 24 = fromHourMin (h `mod` 24) m
    | otherwise = prettyPrint h m
    where
        correctMin h m = fromHourMin (h + (length [x|x <- [1..m], mod x 60 == 0])) (m `mod` 60)
        negativeMin h m = fromHourMin (h - (length [x|x <- [0..(m * (-1))], mod x (60 * (-1)) == 0])) (60 - ((m * (-1)) `mod` 60))
        negativeHour h m 
          | h <= (-24) = fromHourMin (24 - ((h * (-1)) `mod` 24)) m
          | otherwise = fromHourMin (24 + h) m
        prettyPrint h m = Full (fixHour h) (fixMin m)
        
        
        
fixHour :: Int -> String
fixHour h 
    | h < 10 = "0" ++ show h
    | otherwise = show h

fixMin :: Int -> String
fixMin m
    | m < 10 = "0" ++ show m
    | otherwise = show m

