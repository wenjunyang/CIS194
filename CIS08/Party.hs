module Party where

import Data.Tree
import Data.List
import Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp _ f) (GL l fl) = GL (l ++ [emp]) (fl + f)

instance Monoid GuestList where
    mempty = GL [] 0
    mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node n st) = f n $ map (treeFold f) st

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss results = (withBoss, withoutBoss)
  where
    withoutBoss = mconcat (map (uncurry max) results)
    withBoss = glCons boss $ mconcat $ map snd results

maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel

fun :: GuestList -> Fun
fun (GL _ f) = f

guestList :: GuestList -> [Employee]
guestList (GL list f) = list

main :: IO ()
main = do
    company <- readFile "company.txt"
    let guests = maxFun $ read company
    putStrLn $ "Total fun: " ++ (show $ fun guests)
    putStr $ unlines $ sort $ map empName $ guestList guests

