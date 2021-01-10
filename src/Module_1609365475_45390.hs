{-# LANGUAGE ScopedTypeVariables #-}

-- Solution to AdventOfCode2020Problem23b

module Main where

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.List (sort)

d :: Int -> [Int] -> Int -> Int -> Int
d n a l h
  | n `elem` a = d (pred n) a l h
  | n < l      = d h a l h
  | otherwise  = n

rl :: V.Vector a -> V.Vector a
rl xs = V.snoc (V.tail xs) (V.head xs)

move :: Int -> Int -> MV.IOVector Int -> Int -> IO Int
move l h v i = do
    a' <- MV.read v i
    b' <- MV.read v a'
    c' <- MV.read v b'
    d' <- MV.read v c'
    let n = d (pred i) [a',b',c'] l h
    o <- MV.read v n
    MV.write v i d'
    MV.write v n a'
    MV.write v c' o
    MV.read v i

look :: V.Vector Int -> Int
look l = a * b
  where
    a = l V.! 1
    b = l V.! a

iterateM_ :: (Ord t, Num t, Enum t, Monad m) => t -> (a -> m a) -> a -> m a
iterateM_ n f i
    | n < 1     = return i
    | otherwise = f i >>= iterateM_ (pred n) f

prep :: [Int] -> V.Vector Int
prep =
  V.fromList
  . map snd
  . sort
  . (\l -> zip l (tail l))
  . (\l -> 0 : l ++ [succ (Prelude.maximum l) .. 1000000] ++ [head l])

main :: IO ()
main = do
  l@(n:_) <- map (read . return) . head . words <$> getLine
  let v = prep l
  (m :: MV.IOVector Int) <- V.thaw v
  _ <- iterateM_ (10000000 :: Int) (move (V.minimum v) (V.maximum v) m) n
  f <- V.freeze m
  print $ look f

