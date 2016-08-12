module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty            = mempty
tag (Single m a)     = m
tag (Append m a1 a2) = m

--Exercise 1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (mappend (tag jl1) (tag jl2)) jl1 jl2

--Exercise 2
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty                = Nothing
indexJ i (Single s d)         = if i == 0 then Just d else Nothing
indexJ i (Append m jl1 jl2)
    | i < 0 || i >= centerSize = Nothing
    | i < leftSize            = indexJ i jl1
    | otherwise               = indexJ (i - leftSize) jl2
  where
    leftSize   = getSize $ size $ tag jl1
    centerSize = getSize $ size m

-- Exercise 3
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n < 1          = jl
dropJ _ (Single _ _)        = Empty
dropJ n (Append m jl1 jl2)
    | n > centerSize        = Empty
    | n <= leftSize         = dropJ n jl1 +++ jl2
    | otherwise             = dropJ (n - leftSize) jl2
  where
    leftSize   = getSize $ size $ tag jl1
    centerSize = getSize $ size m

-- Exercise 4
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl | n < 1 = Empty
takeJ _ s@(Single _ _) = s
takeJ n jl@(Append m jl1 jl2)
    | n > centerSize        = jl
    | n <= leftSize         = takeJ n jl1
    | otherwise             = jl1 +++ takeJ (n - leftSize) jl2
  where
    leftSize   = getSize $ size $ tag jl1
    centerSize = getSize $ size m




