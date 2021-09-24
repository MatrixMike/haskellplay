module Module_1632377607_38084 where

{-

f x | x == 0 = 3
    | otherwise = 2 * x + f (x - 1)

-- >>> f 10

f' g x
    | x == 0 = 3
    | otherwise = 2 * x + g (x - 1)

-- >>> f' 10 (\x -> f' x (\y -> f' y (\z -> ...)))

f'' = fix f'

-- f'' === f

-- Correct! ... But slow.
-- Complexity = ??? Exponential? Linear? Bad!
fib :: (_) -> Integer -> IO Integer
fib 0 g = pure 1
fib 1 g = 1
fib n g = g (n-1) + g (n-2)

-- fib' = memo fib


-}