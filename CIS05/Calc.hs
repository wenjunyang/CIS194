{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

-- Exercise 1
eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add m n) = (eval m) + (eval n)
eval (ExprT.Mul m n) = (eval m) * (eval n)

-- Exercise 2
evalStr :: String -> Maybe Integer
evalStr = helper . parseExp ExprT.Lit ExprT.Add ExprT.Mul
  where
    helper (Just x) = Just $ eval x
    helper Nothing  = Nothing

-- Exercise 3
class Expr a where
    add :: a -> a -> a
    mul :: a -> a -> a
    lit :: Integer -> a

instance Expr ExprT where
    add = ExprT.Add
    mul = ExprT.Mul
    lit = ExprT.Lit

reify :: ExprT -> ExprT
reify = id

-- Exercise 4
instance Expr Integer where
    add = (+)
    mul = (*)
    lit = id

instance Expr Bool where
    add = (||)
    mul = (&&)
    lit = (>=0)

newtype MinMax = MinMax Integer deriving (Show, Eq)

instance Expr MinMax where
    (MinMax m1) `add` (MinMax m2) = MinMax (max m1 m2)
    (MinMax m1) `mul` (MinMax m2) = MinMax (min m1 m2)
    lit m = MinMax m

newtype Mod7 = Mod7 Integer deriving (Show, Eq)
instance Expr Mod7 where
    (Mod7 m1) `add` (Mod7 m2) = Mod7 (mod (m1 + m2) 7)
    (Mod7 m1) `mul` (Mod7 m2) = Mod7 (mod (m1 * m2) 7)
    lit m = Mod7 (mod m 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
testInteger  = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

-- Exercise 5
instance Expr Program where
    add e1 e2 = e1 ++ e2 ++ [StackVM.Add]
    mul e1 e2 = e1 ++ e2 ++ [StackVM.Mul]
    lit e = [PushI e]

compile :: String -> Maybe Program
compile = parseExp lit add mul :: String -> Maybe Program