{-# LANGUAGE RankNTypes #-}

module Module_1628141420_3219 where

import Control.Lens

data Foo a = Foo
    { tag :: String
    , val :: a
    } deriving Show

data Bar a = Bar
    { bTag :: String
    , bVal :: a
    } deriving Show

lens' :: (s -> a) -> (s -> a -> s) -> Lens' s a
lens' = lens

flensval :: Lens' (Foo a) a
flensval = lens' val (\foo a -> foo {val = a})

blensval :: Lens' (Bar a) a
blensval = lens' bVal (\bar a -> bar {bVal = a})

composedLens :: Lens' (Foo (Bar a)) a
composedLens = flensval . blensval

myfoo :: Foo (Bar Integer)
myfoo = Foo "outer" (Bar "level 1" 123)

view' :: Lens' s a -> s -> a
view' = view

barval :: Integer
barval = myfoo ^. composedLens -- or view composedLens myfoo

newfoo :: Foo (Bar Integer)
newfoo = set composedLens 666 myfoo -- or myfoo & composedLens .~ 666

newfoo2 :: Foo (Bar Integer)
newfoo2 = over composedLens succ myfoo -- or myfoo & composedLens %~ (+1)

-- >>> newfoo2
-- Foo {tag = "outer", val = Bar {bTag = "level 1", bVal = 124}}

-- data Lens s a = Lens
--     { getter :: s -> a
--     , setter :: s -> a -> s
--     }

-- flensval :: Lens (Foo a) a
-- flensval = Lens val (\foo a -> foo {val = a})

-- blensval :: Lens (Bar a) a
-- blensval = Lens bVal (\foo a -> foo {bVal = a})

-- --       (Foo a -> a) -> (a -> b) -> (Foo a -> b)
-- (...) :: Lens (Foo (Bar a)) (Bar a) -> Lens (Bar a) a -> Lens (Foo (Bar a)) a
-- (Lens fget fset) ... (Lens bget bset) =
--     Lens
--         (\foobar -> bget (fget foobar))
--         (\foobar a -> fset foobar (bset (fget foobar) a))

-- nestedLens :: Lens (Foo (Bar a)) a
-- nestedLens = flensval ... blensval

-- -- foo  = Foo "outer" (Foo "level 1" (Foo "level 2" (Foo "inner" 123)))
-- -- foo2 = Foo "outer" (Foo "level 1" (Foo "modified" (Foo "inner" 123)))
-- -- level2_value = tag.val.val
-- -- level2_value_foo = level2_value foo
-- -- updated_foo = foo { val = (val foo { ( val (val foo) {tag = "modifed"})} ) }
-- -- updated_foo_2 = foo2 = copy(foo); foo.data.data.tag = "modified"; return foo2
-- -- Not valid


-- main :: IO ()
-- main = print 1
