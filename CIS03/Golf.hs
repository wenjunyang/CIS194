module Golf where

-- Exercise 1 Hopscotch
skip :: Int -> [a] -> [a]
skip n xs = map fst $ filter (\x -> snd x `mod` n == 0) $ zip xs [1..]
skips :: [a] -> [[a]]
skips xs = map (\x -> skip x xs) index
  where
    index = [1..(length xs)]

-- Exercise 2 Local maxima
localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:ys))
    | y > x && y > z = y : rest
    | otherwise      = rest
    where
      rest = localMaxima xs

localMaxima _        = []

-- Exercise 3 Histogram
-- 统计数字出现次数
calDigit :: [Int] -> [Int]
calDigit xs = map f [0..9]
  where
    f :: Int -> Int
    f x = length (filter (==x) xs)

-- 将出现次数映射成直方图形式的字符串
makeStr :: [Int] -> String
makeStr xs | all (<= 0) xs = ""
           | otherwise     = (makeStr rest) ++ newLine ++ "\n"
           where
             newLine = map (\x -> if x > 0 then '*' else '.') xs
             rest    = map ((-) 1) xs

histogram :: [Int] -> String
histogram xs = (makeStr $ calDigit xs) ++ "==========\n0123456789\n"


