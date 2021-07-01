{-# LANGUAGE InstanceSigs #-}

module Module_1625118584_96472 where

import Control.Applicative
import Control.Monad (join)

-- Applicative
-- Background: Functions
-- Background: Functors

foo :: (a -> b) -> a -> b
foo = undefined

bar :: (a -> b) -> Maybe a -> Maybe b
bar = undefined

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' Maybe where
    fmap' :: (a -> b) -> Maybe a -> Maybe b
    fmap' f (Just x) = Just (f x)
    fmap' _ Nothing  = Nothing

instance Functor' (Either l) where
    fmap' :: (a -> b) -> Either l a {- Left l ... or Right a -} -> Either l b {- Left l ... or Right b -}
    fmap' _ (Left l) = Left l
    fmap' f (Right a) = Right (f a)

class (Functor' f) => Applicative' f where  
    pure' :: x -> f x  
    amap :: f (a -> b) -> f a -> f b  

instance Applicative' Maybe where
    pure' :: a -> Maybe a  
    pure' a = Just a

    amap f a = join' $ fmap' (\g -> fmap' g a) f
    -- amap (Just f) (Just a) = Just (f a)
    -- amap _ _ = Nothing

fmap'' :: (Applicative' f) => (a->b) -> f a -> f b
fmap'' fun arg = amap (pure' fun) arg

--     amap :: f (a -> b) -> f a -> f b  


join' :: Maybe (Maybe a) -> Maybe a
join' (Just (Just a)) = Just a
join' (Just Nothing)  = Nothing
join' Nothing         = Nothing

join'' :: Maybe a -> a
join'' (Just x) = x
join'' Nothing  = error "OOPS!!!"

-- >>> join [[1,2,3], [4,5,6]]
-- [1,2,3,4,5,6]

-- Exercise: Applicative Either - `Either l (a -> b) -> Either l a -> Either l b`

baz :: Maybe (a -> b) -> Maybe a -> Maybe b
baz f a = amap f a



-- newtype === data



--   (a ->   b) -> (  a ->   b) -- Pure function application
--   (a ->   b) -> (f a -> f b) -- Functor
-- f (a ->   b) -> (f a -> f b) -- Applicative
--   (a -> f b) -> (f a -> f b) -- Monad

-- Original bind:
-- m a -> (a -> m b) -> m b 
-- 
-- Specialised Maybe:
-- Maybe a -> (a -> Maybe b) -> Maybe b 

adaptor :: (b -> Maybe c) -> (Maybe b -> Maybe c) -- Monad
adaptor f x = join' (fmap f x)

example :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
example fun1 fun2 a = (adaptor fun2) (fun1 a)

-- Applicative convetion: f <$> g <*> h <*> x

-- do
--   res  <- go
--   res2 <- go2 res
--   pure res2

-- Monad
-- Background: IO ... Do Notation



