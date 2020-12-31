

module Module_1608187994_24372 where

import Data.Array as A

foo :: Array (Integer, Integer) Integer
foo = A.listArray ((1,1),(10,10)) [1..100]

-- >>> foo ! (2,1)
-- 11

