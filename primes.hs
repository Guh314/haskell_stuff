prime :: Int -> Bool
prime 2 = True
prime n = and (map (\x -> if n `mod` x == 0 then False else True) [2..(n-1)])
