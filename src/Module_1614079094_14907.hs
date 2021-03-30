{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}


module Module_1614079094_14907 where

import Data.Foldable ()
import Language.Haskell.TH ()

data Foo a = Bar a | Baz () a deriving (Foldable, Traversable, Functor)

foo :: Foo (Foo (Foo Integer))
foo = Bar (Bar (Baz () 123))

bar :: [Integer]
bar = foldMap (foldMap (foldMap (:[]))) foo

baz :: Maybe Int -> String 
baz Nothing = "hello"
baz (Just _) = []

zzzfoo :: Integer
zzzfoo = 1

alskdjhfas :: Integer
alskdjhfas = 1 + 6

theOp :: [a] -> [a] -> [a]
theOp = $([|(++)|] )

-- blerb suxxxx

mymap :: (blerb->spladonkig) -> [blerb] -> [spladonkig]
mymap _ [] = []
mymap fblerbspladonkig (blerb : l_blerb3)
  = fblerbspladonkig blerb : mymap fblerbspladonkig l_blerb3

-- >>> bar
-- [123]

