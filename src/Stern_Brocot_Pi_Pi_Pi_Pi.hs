module Stern_Brocot_Pi_Pi_Pi_Pi where

import Prelude hiding (Rational, gcd)
import Data.List (unfoldr, last)
import Data.Function (on)

data Nat = Z | S Nat deriving (Show)

minus :: Nat -> Nat -> Nat
minus a Z = a
minus (S a) (S b) = minus a b
minus Z (S _) = error "can't subtract that"

instance Eq Nat where
    Z == Z = True
    (S a) == Z = False
    Z == (S b) = False
    (S a) == (S b) = a == b

instance Ord Nat where
    compare Z Z = EQ 
    compare (S a) Z = GT
    compare Z (S b) = LT
    compare (S a) (S b) = a `compare` b

-- data Int = Neg Nat | Pos Nat

data Frac = Integer :/ Integer deriving Show

instance Eq Frac where
  (x :/ xd) == (y :/ yd) = (x * yd) == (y * xd)
  
instance Ord Frac where
  compare (x :/ xd) (y :/ yd) = compare (x * yd) (y * xd)

instance Num Frac where
  fromInteger n = n :/ 1
  (x :/ xd) * (y :/ yd) = (x * y) :/ (xd * yd)
  (x :/ xd) + (y :/ yd) = (x * yd + y * xd) :/ (xd * yd)
  signum (n :/ d) = signum (n * d) :/ 1
  abs n = signum n * n
  (x :/ xd) - (y :/ yd) = (x * yd - y * xd) :/ (xd * yd)

gcd :: Nat -> Nat -> Nat
gcd n m = case compare n m of
  EQ -> n
  LT -> gcd n (m `minus` n)
  GT -> gcd (n `minus` m) m

-- >>> gcd (S $ S $ Z) (S $ S $ S $ Z)
-- S Z

data Bit = O | I deriving Show

type Rational = [Bit]

newtype Ration = Ration { unRation :: Rational } deriving Show

abs' :: Frac -> Rational
abs' = unfoldr f
  where
    f (n :/ d) = case compare n d of
      EQ -> Nothing
      LT -> Just (O, n :/ (d - n))
      GT -> Just (I, (n - d) :/ d)

-- >>> abs' (7 :/ 13)
-- [O,I,O,O,O,O,O]

rep :: Rational -> Frac
rep = foldr f (1 :/ 1)
  where
    f I (n :/ d) = (n + d) :/ d
    f O (n :/ d) = n :/ (n + d)

-- >>> rep [I,O,I,I,O]
-- 12 :/ 7

data Q
  = Neg Rational
  | Zero
  | Pos Rational 
  deriving Show

instance Num Ration where
  fromInteger n = Ration $ abs' (n :/ 1)
  
  Ration xs + Ration ys = Ration $ abs' (rep xs + rep ys)
  Ration xs * Ration ys = Ration $ abs' (rep xs * rep ys)
  Ration xs - Ration ys = Ration $ abs' (rep xs - rep ys)

infix 6 :-:
data Interval
  = (:-:)
  { lb :: Frac
  , ub :: Frac
  } deriving Show

mediant :: Interval -> Frac
mediant (b :/ d :-: a :/ c) = (a+b) :/ (c+d)

left, right :: Interval -> Interval
left  x = lb x :-: mediant x
right x = mediant x :-: ub x

rep' :: [Bit] -> Frac
rep' = mediant . foldl f ((0 :/ 1) :-: (1 :/ 0))
  where
    f a I = right a
    f a O = left a

-- approximate :: [Bit] -> [Interval]
-- approximate = scanl f mempty
--   where
--     f i I = right i
--     f i O = left  i

data IntervalM a = IM !a !a !a !a deriving Show

lbM, ubM :: IntervalM Integer -> Frac

lbM (IM _ a b _) =  a :/ b
ubM (IM c _ _ d) =  c :/ d

infix 6 --:
(--:) :: Frac -> Frac -> IntervalM Integer
(a :/ b) --: (c :/ d) = IM c a b d

l :: IntervalM Integer
l = IM 1 0 1 1

r :: IntervalM Integer
r = IM 1 1 0 1

-- Matrix Multiplication
(****) :: (Num a) => IntervalM a -> IntervalM a -> IntervalM a
IM a b c d **** IM a' b' c' d' = IM (a*a'+b*c') (a*b'+b*d') (c*a'+d*c') (c*c'+d*d')

-- >>> IM 1 2 3 (4 :: Int) **** IM 5 6 7 8
-- IM 19 22 43 53


rightM :: IntervalM Integer -> IntervalM Integer
rightM x = x **** r

leftM :: IntervalM Integer -> IntervalM Integer
leftM  x = x **** l

memptyM :: IntervalM Integer
memptyM = IM 1 0 0 1

approximate :: [Bit] -> [IntervalM Integer]
approximate = scanl f memptyM
  where
    f i I = rightM i
    f i O = leftM i

m :: Interval -> IntervalM Integer
m (a :/ b :-: c :/ d) = IM c a b d

mediantM :: IntervalM Integer -> Frac
mediantM (IM c a b d) = (a+b) :/ (c+d)

interleave :: (Frac -> Frac -> Frac)
           -> [IntervalM Integer]
           -> [IntervalM Integer]
           -> [IntervalM Integer]
interleave (*) [xi]   ys       = map (\y -> x * lbM y --: x * ubM y) ys where x = mediantM xi
interleave (*) (x:xs) ys@(y:_) = (((*) `on` lbM) x y --: ((*) `on` ubM) x y) : interleave (*) ys xs
interleave _ _ _ = error "invalid list lengths used for interleave"

quad :: (Frac -> Frac -> Frac)
     -> [Bit]
     -> [Bit]
     -> [Bit]
quad (*) xs ys = foldr f (unfoldr p) zs memptyM
  where
    zs = (interleave (*) `on` approximate) xs ys
    
    f x xs c
      | mediantM c < lbM x = I : f x xs (rightM c)
      | mediantM c > ubM x = O : f x xs (leftM  c)
      | otherwise = xs c
        
    t = mediantM (last zs)
    
    p c = case compare (mediantM c) t of
      LT -> Just (I, rightM c)
      GT -> Just (O, leftM  c)
      EQ -> Nothing

add :: [Bit] -> [Bit] -> [Bit]
add [] ys = I : ys
add xs [] = I : xs
add (I:xs) ys = I : add xs ys
add xs (I:ys) = I : add xs ys
add xs ys = quad (+) xs ys

inv :: (Frac -> Frac) -> [Bit] -> [Bit]
inv o n = unfoldr f memptyM
  where
    t = mediantM $ last $ approximate n
    
    f c = case compare (o (mediantM c)) t of
      LT -> Just (I, rightM c)
      GT -> Just (O, leftM  c)
      EQ -> Nothing

sqrtM :: [Bit] -> [Bit]
sqrtM = inv (\x -> x * x)

toDouble :: Frac -> Double
toDouble (a :/ b) = fromIntegral a / fromIntegral b

root2Approx :: [Double]
root2Approx = map (toDouble . mediantM) (approximate (sqrtM (abs' (2 :/ 1))))

-- >>> root2Approx !! 999
-- 1.618033988749895

-- >>> sqrt 2
-- 1.4142135623730951
