{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

-- Exercise 1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 = map fib [0..]

-- Exercise 2
fibs2 = fibgen 0 1
  where fibgen a b = a : fibgen b (a + b)

-- Exercise 3
data Stream a = Cons a (Stream a)
instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons m r) = m : streamToList(r)

-- Exercise 4
streamRepeat :: a -> Stream a
streamRepeat m = Cons m (streamRepeat m)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons m r) = Cons (f m) $ streamMap f r

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f m = Cons m $ streamFromSeed f $ f m

-- Exercise 5
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- 没明白定义
--ruler :: Stream Integer

-- Exercise 6
zeros :: Stream Integer
zeros = streamRepeat 0
x :: Stream Integer
x = Cons 0 $ Cons 1 zeros

instance Num (Stream Integer) where
    fromInteger n = Cons n zeros
    negate = streamMap negate
    (Cons m1 r1) + (Cons m2 r2) = Cons (m1 + m2) (r1 + r2)
    (Cons m1 r1) * c@(Cons m2 r2) = Cons (m1 * m2) rest
      where
        rest = (fromInteger m1) * r2 + r1 * c

divid :: Stream Integer -> Stream Integer -> Stream Integer
c1@(Cons m1 r1) `divid` c2@(Cons m2 r2) = Cons (m1 `div` m2) rest
  where
    rest = streamMap ( `div` m2) (r1 - (c1 `divid` c2) * r2)

fibs3 =  x `divid` (1-x-x^2)

-- Exercise 7
data Matrix = Matrix Integer Integer Integer Integer deriving Show
instance Num Matrix where
    (Matrix m11 m12 m21 m22) * (Matrix n11 n12 n21 n22)
        = Matrix (m11 * n11 + m12 * n21) (m11 * n12 + m12 * n22)
                 (m21 * n11 + m22 * n21) (m21 * n12 + m22 * n22)
    (Matrix m11 m12 m21 m22) + (Matrix n11 n12 n21 n22)
        = Matrix (m11 + n11) (m12 + n12)
                 (m21 + n21) (m22 + n22)

f = Matrix 1 1 1 0
fromMatrix :: Matrix -> Integer
fromMatrix (Matrix _ m _ _) = m
fibs4 :: Integer -> Integer
fibs4 0 = 1
fibs4 n = fromMatrix $ f ^ n

