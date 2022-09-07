{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}

module Module_1648714639_8761 where

import Control.Category ( (>>>), Category(..) )
import Control.Arrow ( Arrow(arr, first) )

newtype A_D a b = A { unA :: Diagram }

data Diagram
    = DName String
    | DComp Diagram Diagram
    | DArr
    | DFirst Diagram
    | DPair Diagram Diagram

instance Category A_D where
    id        = A $ DName "id"
    A f . A g = A $ DComp f g

instance Arrow A_D where
    arr :: FDraw (a -> b) => f -> A_D a b
    arr       f = A DArr
    first (A a) = A (DFirst a)

class FDraw f where
    fdraw :: f ~ (a -> b) => f -> Diagram

instance FDraw (a -> (b,c)) where
    fdraw _ = DPair (DName "->") (DName "->")

instance FDraw (a -> b) where
    fdraw _ = DName "->"

renderD :: Diagram -> String
renderD = \case
    DName n             -> n
    DComp (DFirst f) g -> "(" <> renderD g <> " >>> " <> "(FST " <> renderD f <> "))"
    DComp f g           -> "(" <> renderD g <> " >>> " <> renderD f <> ")"
    DArr                -> "~~~"
    DFirst a            -> "(FST " <> renderD a <> ")"

render :: A_D a b -> String
render (A a) = renderD a

foo :: Arrow a => a (c,d) (c,d)
foo = arr (\x -> x) >>> arr (\x -> x) >>> first (arr (\a -> a))

-- >>> render foo
-- "(~~~ >>> (~~~ >>> (FST ~~~)))"
