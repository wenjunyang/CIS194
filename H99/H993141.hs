-- p31
myIsPrime :: Integer -> Bool
myIsPrime n
    | n <= 1     = False
    | otherwise  = and [(mod n x) /= 0 | x <- 2:[3, 5..q]]
    where
      q = (floor $ sqrt $ fromIntegral n)

-- p32
myGcd :: Integer -> Integer -> Integer
myGcd m n
  | n == 0      = abs m
  | otherwise   = myGcd n (m `mod` n)

-- p33
myIsCoprime :: Integer -> Integer -> Bool
myIsCoprime m = (==1) . (myGcd m)