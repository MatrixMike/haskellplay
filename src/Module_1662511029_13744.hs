{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List.Split
import Data.List (isInfixOf)

main :: IO ()
main = do
  c <- getContents
  let splitter = startsWith "\nState "
  let items = drop 1 $ split splitter c
  let states = map formatState items
  mapM_ putStrLn (concat states)

formatState :: String -> [String]
formatState s = transitions
  where
  ls = takeWhile (/= "-----------------------------------------------------------------------------") $ filter (not . null) (lines s)
  l1 = head ls
  w1 = splitOn " " l1
  s1 = read @Int (w1 !! 1)
  transitions = map (formatTransition s1 . words) $ filter ("and enter state" `isInfixOf`) ls


formatTransition :: Int -> [String] -> String
formatTransition s1 ws = show s1 <> " -> " <> t <> "[label=\"" <> escape f <> "\"];"
  where
  f = head ws
  t = last ws

escape :: [Char] -> [Char]
escape = concatMap unquote

unquote :: Char -> [Char]
unquote '"' = "\\\""
unquote x = [x]