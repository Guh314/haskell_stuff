-- A function that decides if simple logical propositions are always true, meaning a tautoly checker.
-- Using basic values (True, False) and variables (A, B, ..., Z) using negation (~), conjunction (^), implication (=>) and parenthesis.

-- Assoc, lookup table
type Assoc k v = [(k, v)]

-- Function to find a value in a table given a key.
find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k',v) <- t, k == k']

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Imply Prop Prop


-- Parenthesis don't need to be specified within Haskell, since they can specify grouping by themselves.
p1 :: Prop
p1 = And (Var 'A') (Not (Var 'A'))

p2 :: Prop
p2 = Imply (And (Var 'A') (Var 'B')) (Var 'A')

p3 :: Prop
p3 = Imply (Var 'A') (And (Var 'A') (Var 'B'))

p4 :: Prop
p4 = Imply (And (Var 'A') (Imply
         (Var 'A') (Var 'B'))) (Var 'B')


-- Lookup table that associate variable names to logical values
-- ['A', True] or ['B', False]
type Subst = Assoc Char Bool


-- A function that evaluates a proposition given a substitution.
eval :: Subst -> Prop -> Bool
eval _ (Const b)   = b
eval s (Var x)     = find x s
eval s (Not p)     = not (eval s p)
eval s (And p q)   = eval s p && eval s q
eval s (Imply p q) = eval s p <= eval s q


-- Create a list of all variables in the Prop
vars :: Prop -> [Char]
vars (Const _)   = []
vars (Var x)     = [x]
vars (Not p)     = vars p
vars (And p q)   = vars p ++ vars q
vars (Imply p q) = vars p ++ vars q


-- Create a matrix of Bools (overly complex solution)
--bools :: Int -> [[Bool]]
--bools n = map (reverse . map conv . make n . int2bin) range -- where int2bin is taken from the Higher Order Functions chapter.
--            where
--                range     = [0..(n^2)-1]
--                make n bs = take n (bs ++ repeat 0)
--                conv 0    = False
--                conv 1    = True

-- Create a matrix of Bools (simpler solution)
bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss
            where bss = bools (n-1)


-- A function that removes duplicates
rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/=x) (rmdups xs)


-- Using bools and rmdups we create a function that generate all possible substituions for a proposition
substs :: Prop -> [Subst]
substs p = map (zip vs) (bools (length vs))
            where vs = rmdups (vars p)


-- The function that decides if it is a tautology or not.
isTaut :: Prop -> Bool
isTaut p = and [eval s p |  s <- substs p]

