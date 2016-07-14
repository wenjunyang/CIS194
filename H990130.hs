import Data.List

-- p1
myLastRecur :: [a] -> a
myLastRecur [] = error "list is empty"
myLastRecur [x] = x
myLastRecur (_:xs) = myLastRecur xs

myLastFold :: [a] -> a
myLastFold = foldl (\m n -> n) $ error("list is empty")

-- p2
myButLast :: [a] -> a
myButLast [] = error("list is empty")
myButLast [x] = error("only one element")
myButLast [x, _] = x
myButLast (_:xs) = myButLast(xs)

myLastButFold :: [a] -> a
myLastButFold = fst . foldl (\(a, b) x -> (b, x)) (err1, err2)
    where err1 = error("empty")
          err2 = error("only one")

-- p3
elementAt :: (Integral n) => [a] -> n -> a
elementAt x n
    | n == 1    = head x
    | otherwise = elementAt (tail x) (n - 1)

-- p4
myLength :: [a] -> Int
--myLength = foldl (const . (+1)) 0
--myLength = foldr ((+) . const 1) 0
myLength = sum . (map (\x -> 1))


-- p5
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse(xs) ++ [x]

-- p6
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome []  = True
isPalindrome [x] = True
isPalindrome xs  = (head xs == last xs) && isPalindrome(init $ tail xs)

--p7
data NestedList a = Elem a | List [NestedList a] deriving (Show)
myFlatten :: NestedList a-> [a]
myFlatten (Elem e) = [e]
myFlatten (List x) = concatMap myFlatten x

--p8
myCompress :: (Eq a) => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x:y:xs)
    | x == y    = myCompress (y:xs)
    | otherwise = x : myCompress (y:xs)

myCompressFoldr :: (Eq a) => [a] -> [a]
myCompressFoldr = foldr f []
    where
        f x [] = [x]
        f x (y:xs)
            | x == y    = y:xs
            | otherwise = x:y:xs
-- 这个没看懂
--compress xs = foldr f (const []) xs Nothing
--  where
--    f x r a@(Just q) | x == q = r a
--    f x r _ = x : r (Just x)

-- p9
myPack :: Eq a => [a] -> [[a]]
myPack = foldr f []
  where
    f x [] = [[x]]
    f x ((y:ys):xs)
        | x == y    = (x:y:ys):xs
        | otherwise = [x]:(y:ys):xs

-- p10
myEncode :: Eq a => [a] -> [(Int, a)]
myEncode = map (\x -> (length x, head x)) . myPack

-- p11
data MyEcodeList a = Multiple Int a| Single a deriving (Show)
myNewEncode :: (Eq a) => [a] -> [MyEcodeList a]
myNewEncode = map f . myEncode
  where
    f (1, x) = Single x
    f (l, x) = Multiple l x

-- p12
myNewDecode :: (Eq a) => [MyEcodeList a] -> [a]
myNewDecode = foldr f []
  where
    f (Single a) x = (a:x)
    f (Multiple l a) x = (replicate l a) ++ x

-- p13
myEncodeDirect :: (Eq a) => [a] -> [MyEcodeList a]
myEncodeDirect = foldr f []
  where
    f x ((Single y):ys)     | x == y = (Multiple 2 x):ys
    f x ((Multiple l y):ys) | x == y = (Multiple (l+1) y):ys
    f x r                            = (Single x):r

-- p14
myDuplicate :: [a] -> [a]
myDuplicate = foldr (\x r -> x:x:r) []

-- p15
myDuplicateN :: Int -> [a] -> [a]
myDuplicateN n = foldr (\x r -> (replicate n x) ++ r) []

-- p16
myDropEveryN :: Int -> [a] -> [a]
myDropEveryN n = snd . (foldl f (n, []))
  where
    f (1, r) x = (n, r)
    f (m, r) x = (m - 1, r ++ [x])

-- p17
mySplitN :: [a] -> Int -> ([a], [a])
mySplitN (x:xs) n
    | n > 0 = (x:th, ts)
    where (th, ts) = mySplitN xs (n - 1)
mySplitN xs n = ([], xs)

mySplitNLet :: [a] -> Int -> ([a], [a])
mySplitNLet (x:xs) n
    | n > 0 = let (th, ts) = mySplitNLet xs (n-1) in (x:th, ts)
mySplitNLet xs n = ([], xs)

-- p18
mySlice :: [a] -> Int -> Int -> [a]

mySlice (x:xs) start end
    | start > end            = []
    | end < 1                = []      -- 无穷list可以提前终结
    | start <= 1             = x:t
    | otherwise              = t
    where t = mySlice xs (start - 1) (end - 1)
mySlice [] _ _ = []

-- p19
myRotate :: [a] -> Int -> [a]
myRotate xs n
    | (abs n) > len = error("illegal argument")
    | otherwise     = r ++ l
    where
      (l, r) = splitAt (mod (len + n) len) xs
      len = length xs

-- p20
myRemoveAt :: Int -> [a] -> (a, [a])
myRemoveAt 1 (x:xs) = (x, xs)
myRemoveAt n (x:xs) = (tl, x:ts)
  where (tl, ts) = myRemoveAt (n-1) xs

main :: IO()
main = putStrLn("h")
