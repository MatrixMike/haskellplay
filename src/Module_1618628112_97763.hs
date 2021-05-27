{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Module_1618628112_97763 where

import GHC.Generics
import Data.Data
import Control.Concurrent.STM

-- import Type.Reflection
-- import Data.Typeable

data Foo = Foo
    { bar :: Int 
    , baz :: String 
    }
    deriving (Generic, Typeable, Data)

-- >>> typeRep (Proxy :: Proxy Foo)
-- Foo

foo :: Foo
foo = Foo 1 "asdf"

-- >>> map constrFields $ dataTypeConstrs $ dataTypeOf (undefined :: Foo)
-- [["bar","baz"]]


