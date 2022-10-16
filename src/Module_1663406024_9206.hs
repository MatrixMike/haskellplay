module Module_1663406024_9206 where

import Math.NumberTheory.Primes (Prime(unPrime), primes)

-- $setup
-- >>> import qualified Numeric.AD as Ad
-- >>> import qualified Data.Number.Symbolic as Sym

-- >>> f x = x^2 + 3 * x
-- >>> Ad.diff f 1
-- >>> Ad.diff f (Sym.var "a")
-- 5
-- 3+a+a

-- >>> Ad.diff sin pi
-- >>> Ad.diff sin (Sym.var "a")
-- -1.0
-- cos a

-- >>> y = 9
-- >>> Ad.diff (\x -> x^2 + sqrt y) 1
-- 2.0

-- >>> y = 5
-- >>> Ad.diff (\x -> x^2 + sqrt y) (Sym.var "a")
-- a+a
-- 
-- a+a+1.0/(2.0*sqrt a)

zetaP :: Floating a => Int -> a -> a
zetaP n s = product (map f ps)
  where
  f p = 1 / (1 - p ** negate s)
  ps = map (fromIntegral . unPrime) $ take n (primes :: [Prime Integer])

zetaS :: Floating a => Int -> a -> a
zetaS n s = sum (map f ns)
  where
  f x = 1 / (fromIntegral x ** s)
  ns = take n [(1 :: Integer) ..]

-- | Testing zetaP
-- 
-- >>> zetaP 100000 5
-- 1.036927755143382

-- >>> zetaS 100000 5
-- 1.036927755143338


fact :: (Num a, Enum a) => a -> a
fact n = product [1..n]

facts :: [Integer]
facts = 1 : 2 : zipWith (*) [3..] (tail facts)

-- >>> take 5 facts
-- [1,2,6,24,120]

diff :: Fractional a => [a] -> [a]
diff xs = zipWith (/) xs (tail xs)

factDiffs :: [[Double]]
factDiffs = iterate diff (fromIntegral <$> facts)

-- >>> take 4 $ map (take 5) factDiffs
-- [[1.0,2.0,6.0,24.0,120.0],[0.5,0.3333333333333333,0.25,0.2,0.16666666666666666],[1.5,1.3333333333333333,1.25,1.2000000000000002,1.1666666666666667],[1.125,1.0666666666666667,1.0416666666666665,1.0285714285714287,1.0208333333333335]]

-- >>> fact 1
-- >>> fact 2
-- >>> fact 3
-- >>> fact 4
-- >>> fact 5
-- >>> fact 7
-- >>> fact 9
-- >>> fact 11
-- 1
-- 2
-- 6
-- 24
-- 120
-- 5040
-- 362880
-- 39916800
