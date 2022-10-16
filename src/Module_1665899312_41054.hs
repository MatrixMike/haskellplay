{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveFunctor #-}

-- | From https://www.reddit.com/r/haskell/comments/y4n5ah/summing_polynomials_in_haskell/

module Module_1665899312_41054 where

instance Num a => Num (x -> a) where
  f + g = \x -> f x + g x
  f * g = \x -> f x * g x
  negate f = \x -> negate (f x)
  abs f = \x -> abs (f x)
  signum f = \x -> signum (f x)
  fromInteger n = \_ -> fromInteger n

class (Num r, Num a) => Algebra r a where
  scale :: r -> a -> a

instance Num r => Algebra r r where
  scale = (*)

newtype P r x = P (forall a. Algebra r a => (x -> a) -> a)
  deriving Functor

instance Num (P r x) where
  P p1 + P p2 = P (p1 + p2)
  P p1 * P p2 = P (p1 * p2)
  negate (P p) = P (negate p)
  abs (P p) = P (abs p)
  signum (P p) = P (signum p)
  fromInteger n = P (fromInteger n)

instance Num r => Algebra r (P r x) where
  scale r (P p) = P (scale r . p)

data Witness c a where
  Witness :: forall c a. c a => Witness c a

class Functor f => Free c f | f -> c where
  free :: Witness c (f x)
  var :: x -> f x
  eval :: c a => (x -> a) -> f x -> a

instance Num r => Free (Algebra r) (P r) where
  free = Witness
  var x = P (\d -> d x)
  eval d (P p) = p d

data PExpr r x
  = Var x
  | Sca r (PExpr r x)
  | Add (PExpr r x) (PExpr r x)
  | Mul (PExpr r x) (PExpr r x)
  | Neg (PExpr r x)
  | Abs (PExpr r x)
  | Sig (PExpr r x)
  | Lit Integer

expr :: Num r => P r x -> PExpr r x
expr = eval Var

instance Num (PExpr r x) where
  (+) = Add
  (*) = Mul
  negate = Neg
  abs = Abs
  signum = Sig
  fromInteger = Lit

instance Num r => Algebra r (PExpr r x) where
  scale = Sca

instance (Show r, Show x) => Show (PExpr r x) where
  show e =
    let
      op :: (Show a, Show b) => String -> a -> b -> String
      op sym e1 e2 = unwords [par e1, sym, par e2]

      par :: Show a => a -> String
      par e = "(" <> show e <> ")"

    in case e of
      Var x -> show x
      Sca r e -> op "." r e
      Add e1 e2 -> op "+" e1 e2
      Mul e1 e2 -> op "*" e1 e2
      Neg e -> "-" <> par e
      Abs e -> "abs" <> par e
      Sig e -> "signum" <> par e
      Lit n -> show n

vx :: Num r => P r String
vx = var "x"

vy :: Num r => P r String
vy = var "y"

-- 2x^2 + y + 2
p1x :: Num r => P r String
p1x = 2 * vx ^ 2 + vy + 2

-- 2y^3 + 4x^2 + 3y^2 + 3
p2x :: Num r => P r String
p2x = 2 * vy ^ 3 + 4 * vx ^ 2

-- >>> expr p2x

-- (2x^2 + y + 2) + (2y^3 + 4x^2 + 3y^2 + 3)
px :: Num r => P r String
px = p1x + p2x

-- >>> expr px
-- ((((2) * (("x") * ("x"))) + ("y")) + (2)) + (((2) * ((("y") * ("y")) * ("y"))) + ((4) * (("x") * ("x"))))

main :: IO ()
main = print (expr px)
