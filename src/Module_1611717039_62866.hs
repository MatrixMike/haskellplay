
module Module_1611717039_62866 where

import Data.Semigroup.Monad
-- import Data.Monoid.Endo

pp :: Show a => a -> IO [a]
pp x = print x >> return [x]

foo :: IO [Int]
foo = getMon $ (Mon . pp <> Mon . pp <> Mon . pp) 1

-- >>> foo
-- [1,1,1]

