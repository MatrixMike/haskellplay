{-# language TypeFamilies #-}

class Foo (a :: * -> *) where
    data Bar a :: * -> *
    data Baz a
    foo :: a x -> Bar a x

-- instance Foo (Either a) where
    -- data Bar Int = Bla Int
    -- data Baz Int = Qux Int
    -- foo i = Bla i

instance Foo Maybe where
    data Bar Maybe x = MBar x
    data Baz Maybe   = MBaz
    foo (Just x)     = MBar x

test :: Maybe x -> x
test x = foo x


