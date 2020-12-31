
module Main where

import qualified Data.Set as S
import Control.Arrow ( Arrow((&&&)) )
import Data.List.Split ( splitOn )

f :: S.Set ([Int], [Int]) -> ([Int], [Int]) -> ([Int], [Int])
f _ h@([],_) = h
f _ h@(_,[]) = h
f p h@(a:b,c:d)
    | S.member h p = fp (b ++ [a,c], d)
    | a <= length b && c <= length d = case f S.empty (take a b, take c d) of
        (_,[]) -> fp (b ++ [a,c], d)
        _      -> fp (b, d ++ [c,a])
    | a > c     = fp (b ++ [a,c], d)
    | otherwise = fp (b, d ++ [c,a])
    where
    fp = f (S.insert h p)

main :: IO ()
main = interact $
    show
    . sum
    . zipWith (*) [1..]
    . reverse
    . uncurry (++)
    . f S.empty
    . (head &&& last)
    . map (tail . map read)
    . splitOn [[]]
    . lines
