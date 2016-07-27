module CIS04 where

-- Exercise 1: Wholemeal programming
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
     | even x    = (x - 2) * fun1 xs
     | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldl (*) 1 . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

-- Exercise 2: Folding with trees
data Tree a = Leaf
             | Node Integer (Tree a) a (Tree a)
   deriving (Show, Eq)

treeDepth :: Tree a -> Integer
treeDepth Leaf = -1
treeDepth (Node h _ _ _) = h

makeNode :: Tree a -> a -> Tree a -> Tree a
makeNode left d right = Node h left d right
  where
    h = 1 + (max (treeDepth left) (treeDepth right))

foldTree :: [a] -> Tree a
foldTree = foldr helper Leaf
  where
    helper :: a -> Tree a -> Tree a
    helper x Leaf = Node 0 Leaf x Leaf
    helper x (Node h left d right)
        | (treeDepth left) > (treeDepth right) = makeNode left d (helper x right)
        | otherwise                            = makeNode (helper x left) d right

-- Exercise 3: More folds!
xor :: [Bool] -> Bool
xor = foldl (/=) True

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\a z -> f a : z) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (\a z -> f z a) base . reverse

-- Exercise 4: Finding primes
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = filter (\a -> not $ elem a sieveList) [3, 5..(2 * n + 1)]
  where
    sieveList = [2 * (x + y + (2 * x * y)) + 1 | x <- [1..n], y <- [x..n], x + y + (2 * x * y) <= n]
