
----------------------------------------
-- 卡片数字校验
----------------------------------------
toDigits         :: Integer -> [Integer]
toDigitsRev      :: Integer -> [Integer]
doubleEveryOther :: [Integer] -> [Integer]
sumDigits        :: [Integer] -> Integer
validate         :: Integer -> Bool

toDigits n
    | n <= 0    = []
    | otherwise = (toDigits m) ++ [r]
    where (m, r) = n `divMod` 10

toDigitsRev = reverse . toDigits

doubleEveryOther = fst . (foldr helper ([], 1))
  where
    helper x (rs, 1) = (x : rs, 2)
    helper x (rs, 2) = ( 2 * x : rs, 1)

sumDigits = sum . (map (sum . toDigits))

validate digit =
  let
    checkSum = (sumDigits . doubleEveryOther . toDigits) digit
  in
    checkSum `mod` 10 == 0

----------------------------------------
-- hanoi游戏
----------------------------------------
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n - 1) a c b) ++ [(a, b)] ++ (hanoi (n - 1) c b a)