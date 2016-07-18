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


