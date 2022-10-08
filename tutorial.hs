-- This is a basic learning of Haskell

{- Lazy language:
    Only evaluates what it needs.
-}

-- Imports
import Data.List

-- :: Bool
trueAndFalse = True && False
trueOrFalse = True || False
notTrue = not(True)

-- :: Int
maxInt = maxBound :: Int

-- :: Integer
numFive :: Integer
numFive = 5

-- :: Float
numFive' :: Float
numFive' = 5.0

boolFive :: Bool
boolFive = 5 < 4

-- :: Double
-- if not specified
myFloat = 1.0 + 2.5
myDouble = 3.0 + 1.0
myFloat' :: Float
myFloat' = 1.0 + 2.5

-- :: Char
myChar = 'a'

-- :: String
myString = "String"

-- Math
mySum = 2 + 3
myDiv = 6 / 3
mySub = 5 - 2
myMult = 3 * 6
myMod = 9 `mod` 3

-- pi, exp, log, sin
truncDouble = truncate myDouble
roundDouble = round myDouble
ceilDouble = ceiling myDouble
floorDouble = floor myDouble

mySqrt = sqrt myFloat

-- List
numList = [1,2,3,4]
rangeList = [1..5]
alphaList = ['a'..'z']
evenNums = [2,4..20]
oddNums = [1,3..15]

sumNumList = sum numList
