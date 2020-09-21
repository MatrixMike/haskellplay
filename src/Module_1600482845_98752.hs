module Module_1600482845_98752 where

main :: IO ()
main = do
    putStrLn ""
    putStrLn ""
    putStrLn (exclaim "Hello Software Freedom Day Melbourne")
    putStrLn ""
    putStrLn (exclaim "I'm going to talk about MHUG")
    putStrLn ""
    printAll (map exclaim (words conclusion))
    putStrLn ""
    putStrLn ""

    putStrLn $ format $ (fmap (fmap show)) $ take 21 triangle

format :: [[String]] -> String
format ss = unlines $ zipWith (indent height biggest) [0..] $ fmap (unwords . fmap (pads biggest)) ss
    where
        biggest = maximum $ map length $ concat ss
        height  = length ss

indent :: Int -> Int -> Int -> String -> String
indent h b i s = pad (b * (h - i) `div` 2) s

exclaim :: String -> String
exclaim w = w ++ "!"

conclusion :: String
conclusion = "Let's Get Started"

printAll :: [String] -> IO ()
printAll [] = return ()
printAll (x:xs) = do
    putStrLn x
    printAll xs

pads :: Int -> String -> String
pads i s = reverse $ take i $ reverse s ++ repeat ' '

pad :: Int -> String -> String
pad x y = replicate x ' ' ++ y

triangle :: [[Integer]]
triangle = iterate pascal [1]

pascal :: Num a => [a] -> [a]
pascal row = addRows ([0] ++ row) (row ++ [0])

addRows :: Num c => [c] -> [c] -> [c]
addRows r1 r2 = zipWith (+) r1 r2

{-
          1
        1   1
      1   2   1
    1   3   3   1
  1   4   6   4   1
1   5   10  10  5   1
-}