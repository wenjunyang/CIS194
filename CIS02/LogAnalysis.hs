{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE ViewPatterns #-}


module LogAnalysis where

import Log

isNumberic :: String -> Bool
isNumberic s = case reads s :: [(Int, String)] of
    [(_, "")] -> True
    _         -> False

parseMessage :: String -> LogMessage
parseMessage msg = (par . splitAt 2 . words) msg
  where
    par (["E", n], xs) | (isNumberic n) && (isNumberic $ head xs) =
        LogMessage (Error (read n :: Int)) (read (head xs) :: Int) (unwords $ tail xs)
    par (["I", n], xs) | isNumberic n =
        LogMessage Info (read n :: Int) (unwords xs)
    par (["W", n], xs) | isNumberic n =
        LogMessage Warning (read n :: Int) (unwords xs)
    par _ = Unknown msg

parse :: String -> [LogMessage]
parse = map parseMessage . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree      = tree
insert msg Leaf              = Node Leaf msg Leaf
insert (msg) (Node l m r)
    | msg > m     = Node l m (insert msg r)
    | otherwise   = Node (insert msg l) m r

build :: [LogMessage] -> MessageTree
build = foldr (\a z -> insert a z) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map messageOf . filter isSerious . inOrder . build
  where
    messageOf (LogMessage (Error _) _ info)  = info
    messageOf _                              = error("unexpected argument")
    isSerious (LogMessage (Error level) _ _) = level > 50
    isSerious _                              = False


