{-# LANGUAGE BlockArguments #-}

module Module_1630906808_32351 where

-- What is a property?
-- Something that holds true for all inputs

import Test.QuickCheck
import Data.Ratio
import Control.Monad

my_prop_1 :: Bool
my_prop_1 = undefined 

my_prop_2 :: (Arbitrary a) => a -> Bool
my_prop_2 = undefined

my_prop_3 :: Property 
my_prop_3 = undefined 

--  Testable


-- Round-trip: Ex: reverse.reverse = id
-- Injective: f A -> B `elems` [a,b,c...] 
-- Test parser: String -> Parse it -? Render it -> Original string??? hard --- Bad examepl
-- Test parser: AST -> Render -> Parse AST = Nomalised AST --- Good example
-- Test parser: AST -> Render -> Parse AST = AST --- Good example
-- Negative properties: Useful to check that thins break in the expected way
-- Partial applications
-- Mocked scenarios - 



invert_number :: Fractional a => a -> a
invert_number n = 1/n

prop_invert_number_roundtrip :: Ratio Int -> Bool 
prop_invert_number_roundtrip n = invert_number (invert_number n) == n

-- >>> quickCheck prop_invert_number_roundtrip


prop_invert_number_roundtrip2 :: Ratio Int -> Property 
prop_invert_number_roundtrip2 n =
    n /= 0 ==> invert_number (invert_number n) == n

-- >>> quickCheck prop_invert_number_roundtrip2

prop_invert_number_roundtrip3 :: Ratio Int -> Bool
prop_invert_number_roundtrip3 n = invert_number (invert_number n) == n

prop_invert_number_roundtrip4 :: Property
prop_invert_number_roundtrip4 = forAll foo prop_invert_number_roundtrip3

nonzero :: Gen Int
nonzero = do
    x <- chooseAny
    if x == 0
        then nonzero
        else pure x

foo :: Gen (Ratio Int)
foo = do
    num <- chooseAny
    den <- nonzero
    pure (num % den)

-- >>> quickCheckResult prop_invert_number_roundtrip4
-- Success {numTests = 100, numDiscarded = 0, labels = fromList [([],100)], classes = fromList [], tables = fromList [], output = "+++ OK, passed 100 tests.\n"}

-- >>> quickCheckResult True
-- Success {numTests = 1, numDiscarded = 0, labels = fromList [([],1)], classes = fromList [], tables = fromList [], output = "+++ OK, passed 1 test.\n"}


-- Shrinking

newtype Foo = Foo String deriving Show


instance Arbitrary Foo where
    arbitrary = do
        l <- chooseInt (0,100)
        s <- replicateM l (elements ['a'..'z'])
        pure $ Foo s

    shrink (Foo []) = []
    shrink (Foo s@(_:as)) = [Foo as, Foo (init s)]

prop_silly_string :: Foo -> Bool
prop_silly_string (Foo s) = 'a' `notElem` s

-- >>> quickCheckResult prop_silly_string
-- Failure {numTests = 1, numDiscarded = 0, numShrinks = 69, numShrinkTries = 3, numShrinkFinal = 2, usedSeed = SMGen 12270441333170819766 17635438009789182031, usedSize = 0, reason = "Falsified", theException = Nothing, output = "*** Failed! Falsified (after 1 test and 69 shrinks):\nFoo \"a\"\n", failingTestCase = ["Foo \"a\""], failingLabels = [], failingClasses = fromList []}

