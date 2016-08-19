module Party where

import Data.Tree
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ f) (GL l fl) = GL (l ++ [emp]) (fl + f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

treeFold :: (b -> a -> b) -> b -> Tree a -> b
treeFold f s (Node n st) = foldl (treeFold f) (f s n) st



