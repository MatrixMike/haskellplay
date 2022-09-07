{-# LANGUAGE TypeApplications #-}

module Module_1638427888_33111 where

import Prelude hiding ((.), id)

import Control.Category

import Control.Monad

import Control.Arrow


foo :: (a -> b) -> (b -> c) -> (a -> c)
foo f g = g . f

foo' :: Monad m => Kleisli m a b -> Kleisli m b c -> Kleisli m a c
foo' f g = g . f

foo'' :: Monad m => Kleisli m a a
foo'' = id

foo''' :: Monad m => Kleisli m a b -> Kleisli m (a,x) (b,x)
foo''' f = first f


{-

class Category cat where
    -- | the identity morphism
    id :: cat a a

    -- | morphism composition
    (.) :: cat b c -> cat a b -> cat a c

    {-# RULES
    "identity/left" forall p .
                    id . p = p
    "identity/right"        forall p .
                    p . id = p
    "association"   forall p q r .
                    (p . q) . r = p . (q . r)
    #-}


class Category a => Arrow a where
    {-# MINIMAL arr, (first | (***)) #-}

    -- | Lift a function to an arrow.
    arr :: (b -> c) -> a b c

    -- | Send the first component of the input through the argument
    --   arrow, and copy the rest unchanged to the output.
    first :: a b c -> a (b,d) (c,d)
    first = (*** id)

    -- | A mirror image of 'first'.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    second :: a b c -> a (d,b) (d,c)
    second = (id ***)

    -- | Split the input between the two argument arrows and combine
    --   their output.  Note that this is in general not a functor.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (***) :: a b c -> a b' c' -> a (b,b') (c,c')
    f *** g = first f >>> arr swap >>> first g >>> arr swap
      where swap ~(x,y) = (y,x)

    -- | Fanout: send the input to both argument arrows and combine
    --   their output.
    --
    --   The default definition may be overridden with a more efficient
    --   version if desired.
    (&&&) :: a b c -> a b c' -> a b (c,c')
    f &&& g = arr (\b -> (b,b)) >>> f *** g

    {-# RULES
    "compose/arr"   forall f g .
                    (arr f) . (arr g) = arr (f . g)
    "first/arr"     forall f .
                    first (arr f) = arr (first f)
    "second/arr"    forall f .
                    second (arr f) = arr (second f)
    "product/arr"   forall f g .
                    arr f *** arr g = arr (f *** g)
    "fanout/arr"    forall f g .
                    arr f &&& arr g = arr (f &&& g)
    "compose/first" forall f g .
                    (first f) . (first g) = first (f . g)
    "compose/second" forall f g .
                    (second f) . (second g) = second (f . g)
    #-}


-}


bar :: Arrow a => a b c
bar = undefined

main :: IO ()
main = print @Int 1





