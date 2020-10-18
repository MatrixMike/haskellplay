module Eulers_pentagonal_formula where

-- Solution to euler problem from https://www.youtube.com/watch?v=iJ8pnCO0nTY
-- The hardest "What comes next?" (Euler's pentagonal formula)

-- >>> partitions !! 600 !! 600
-- 458004788008144308553622

-- >>> partitions !! 10
-- [1,1,2,3,5,7,11,15,22,30,42]

partitions :: [[Integer]]
partitions = seq9
    where
    seq1   = [1..]
    seq2   = [3,5..]
    seq3   = concat $ zipWith (\a b -> [a,b]) seq1 seq2
    seq4   = cycle [1,1,-1,-1]
    seq5   = zip seq4 seq3
    seq6   = seq5 >>= (\(x,y) -> x : replicate (pred y) 0)
    seq7 l = sum $ zipWith (*) seq6 l
    seq8   = iterate (\l -> seq7 l : l) [1]
    seq9   = map reverse seq8

