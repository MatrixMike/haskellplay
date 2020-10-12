{-# LANGUAGE BlockArguments #-}

module StupidChurroBubbleSort where

import Control.Churro
import System.Random
import Prelude hiding (id)

bubblesort :: Ord a => [a] -> IO [a]
bubblesort = eqfix walk

eqfix :: Eq a => (a -> IO a) -> a -> IO a
eqfix f a = do
    b <- f a
    if a == b
        then return a
        else eqfix f b

walk :: Ord a => [a] -> IO [a]
walk []     = pure []
walk (x:xs) = ((x:) <$> step xs) >>= step

step :: Ord a => [a] -> IO [a]
step [] = return []
step xs = runWaitListChan $ sourceList xs >>> chunksOf 2 >>> arr order >>> mapN id

chunksOf' :: Transport t => Natural -> t (Maybe a) -> IO [a]
chunksOf' n i
    | n < 1 = return []
    | otherwise = do
        x <- yank i
        case x of
            Nothing -> return []
            Just x' -> (x':) <$> chunksOf' (pred n) i

chunksOf :: Transport t => Natural -> Churro t a [a]
chunksOf n = buildChurro \i o -> prog i o
    where
    prog i o = do
        x <- chunksOf' n i
        yeet o (Just x)
        if length x < fromIntegral n
            then yeet o Nothing
            else prog i o

order :: Ord b => [b] -> [b]
order []    = []
order [x]   = [x]
order (x:y:xs) | x < y     = x : y : xs
               | otherwise = y : x : xs

main :: IO ()
main = do
    xs <- take 25 . randomRs (1 :: Int, 1000) <$> newStdGen
    print xs
    print =<< step xs
    print =<< walk xs
    print =<< bubblesort xs
