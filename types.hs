-- Type and Data Declarations



-- Type Declarations



-- Type String as alias to [Char]
-- type String = [Char]


-- Type that defines a position (x,y)
type Pos = (Int, Int)


-- Type that defines a pair
type Pair a = (a,a)


-- Associative Type Declaration
type Assoc k v = [(k,v)]

-- A function that returns a value associated to a key in a table
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

--table :: forall k v. Assoc k v
table = [(14, 24), (1, 50), (34, 59)]



-- Data Declaration



-- Boolean Data Declaration Example
-- data Bool = False | True


-- Data that represents the moves
data Move = South
          | North 
          | East 
          | West

-- Function that applies a move to a position
move :: Move -> Pos -> Pos
move North (x,y) = (x,y+1)
move South (x,y) = (x,y-1)
move East (x,y)  = (x-1,y)
move West (x,y)  = (x+1,y)

-- Function that applies a list of moves to a position
moves :: [Move] -> Pos -> Pos
--moves ms p = foldl (flip moves) p ms
moves [] p     = p
moves (m:ms) p = moves ms (move m p)


-- Function that reverse the direction of a move
rev :: Move -> Move
rev North = South
rev South = North
rev East  = West
rev West  = East


-- A data that represents shapes
data Shape = Circle Float
           | Rect Float Float

-- Produces a square given a value
square :: Float -> Shape
square n = Rect n n 

-- Calculates the area of a shape
area :: Shape -> Float
area (Rect x y) = x * y
area (Circle r) = pi * r^2


-- Maybe data Declaration
--data Maybe a = Nothing | Just a

-- With Maybe a safe div can be declared
safediv :: Int -> Int -> Maybe Int
safediv _ 0 = Nothing
safediv m n = Just (m `div` n)

-- A safehead can also be defined
safehead :: [a] -> Maybe a
safehead [] = Nothing
safehead xs = Just (head xs)



-- Newtype Declaration



-- Declares a newtype that inherits Int but it's different
-- newtype Nat = N Int



-- Recursive Type Declarations



-- Natural Numbers
data Nat = Zero | Succ Nat


nat2int :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n


int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))


addNat :: Nat -> Nat -> Nat
addNat m n = int2nat (nat2int m + nat2int n)

--Values to use as test
val1 :: Nat
val1 = int2nat 10

val2 :: Nat
val2 = int2nat 15


add :: Nat -> Nat -> Nat
add Zero n     = n
add (Succ m) n = Succ (add m n)


-- List
data List a = Nil | Cons a (List a)

len :: List a -> Int
len Nil = 0
len (Cons _ xs) = 1 + len xs


-- Tree Data
data Tree a = Leaf a | Node (Tree a) a (Tree a)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
     (Node (Leaf 6) 7 (Leaf 9))


occurs :: Eq a => a -> Tree a -> Bool
occurs x (Leaf y)     = x == y
occurs x (Node l y r) = x == y || occurs x l || occurs x r


flatten :: Tree a -> [a]
flatten (Leaf x)     = [x]
flatten (Node l x r) = flatten l ++ [x] ++ flatten r


occurs' :: Ord a => a -> Tree a -> Bool
occurs' x (Leaf y)              = x == y
occurs' x (Node l y r) | x == y = True
                       | x < y  = occurs' x l
                       | x > y  = occurs' x r
