{-# LANGUAGE TupleSections #-}

module Module_1608096364_4022 where

import Data.List
import Data.Array
import Data.Function ((&))


finds :: Eq a => a -> [(a, b)] -> [b]
finds s = map snd . filter ((==s) . fst)

edges :: (t -> t -> Bool) -> [t] -> [(t, t)]
edges p l = q =<< tails l
    where
    q (x:xs) = map (x,) $ takeWhile (p x) xs
    q _ = []

-- >>> [1 :: Int ..20] & edges (\a b -> b <= a + 3)
-- [(1,2),(1,3),(1,4),(2,3),(2,4),(2,5),(3,4),(3,5),(3,6),(4,5),(4,6),(4,7),(5,6),(5,7),(5,8),(6,7),(6,8),(6,9),(7,8),(7,9),(7,10),(8,9),(8,10),(8,11),(9,10),(9,11),(9,12),(10,11),(10,12),(10,13),(11,12),(11,13),(11,14),(12,13),(12,14),(12,15),(13,14),(13,15),(13,16),(14,15),(14,16),(14,17),(15,16),(15,17),(15,18),(16,17),(16,18),(16,19),(17,18),(17,19),(17,20),(18,19),(18,20),(19,20)]


-- paths' :: (Num p, Eq a) => [(a, a)] -> a -> a -> p
-- paths' l' t' f'
--     | f' == t' = 1
--     | otherwise = sum $ map (paths' l' t') (finds f' l')

paths :: (Ix i, Num e, Enum i) => i -> i -> [(i, i)] -> e
paths f t l =
    let
        a = array (f,t) ((t,1) : [(x, sum $ map (a!) (finds x l)) | x <- [f.. pred t]])
    in
    a ! f

-- >>> [1 :: Int ..20] & edges (\a b -> b <= a + 3) & paths 1 20
-- 66012

-- >>> g (\a b -> b < a + 3) td
-- [[(1,2),(1,3)],[(2,3),(2,4)],[(3,4),(3,5)],[(4,5),(4,6)],[(5,6),(5,7)],[(6,7),(6,8)],[(7,8),(7,9)],[(8,9),(8,10)],[(9,10)],[],[]]


fibs :: (Ix i, Num i, Num e, Enum i) => i -> e
fibs n =
    let a = array (0,n) ((0,1) : (1,1) : map (\x -> (x, (a ! pred x) + (a ! pred (pred x)))) [2..n])
    in a ! n

-- >>> fibs 100
-- 573147844013817084101
