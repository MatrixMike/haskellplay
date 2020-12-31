

module Module_1608330288_52905 where

import Data.Function

smallInput :: String
smallInput = "F10 N3 F7 R90 F11"

answer :: Integer
answer = 25

start :: (Integer, Integer, Char)
start = (0,0,'E')

moveNorth :: Num b => b -> (a, b, c) -> (a, b, c)
moveNorth distance (x,y,d) = (x,y+distance,d)

moveEast :: Num a => a -> (a, b, c) -> (a, b, c)
moveEast distance (x,y,d) = (x+distance,y,d)

stay :: (a, b, c1) -> (a, b, c1)
stay (x,y,d) = (x,y,d)

turnLeft :: (a, b, Char) -> (a, b, Char)
turnLeft (x,y,d) = (x,y,tl d)

tl :: Char -> Char
tl 'E' = 'N'
tl 'S' = 'E'
tl 'W' = 'S'
tl 'N' = 'W'

goFoward :: Num a => a -> (a, a, Char) -> (a, a, Char)
goFoward n (x,y,d@'E') = (x+n,y,d)
goFoward n (x,y,d@'S') = (x,y-n,d)
goFoward n (x,y,d@'W') = (x-n,y,d)
goFoward n (x,y,d@'N') = (x,y+n,d)

rules :: (Num a, Eq a) => (Char, a) -> (a, a, Char) -> (a, a, Char)
rules ('N', n  ) = moveNorth n
rules ('S', n  ) = moveNorth (negate n)
rules ('E', n  ) = moveEast n
rules ('W', n  ) = moveEast (negate n)
rules ('L', 0  ) = stay
rules ('L', 90 ) = turnLeft
rules ('L', 180) = turnLeft . turnLeft
rules ('L', 270) = turnLeft . turnLeft . turnLeft
rules ('R', 0  ) = stay
rules ('R', 90 ) = turnLeft . turnLeft . turnLeft
rules ('R', 180) = turnLeft . turnLeft
rules ('R', 270) = turnLeft
rules ('F', n  ) = goFoward n

displacement :: Num a => (a, a, c) -> a
displacement (x,y,d) = abs x + abs y

decodeRule :: Read b => [Char] -> (Char, b)
decodeRule (letter:number) = (letter, read number)

decodeRules :: String -> [(Char, Integer)]
decodeRules input = map decodeRule $ words input

decodeRules2 :: String -> (Integer, Integer, Char) -> (Integer, Integer, Char)
decodeRules2 = foldr (.) id . map rules . decodeRules

decodeRules3 :: String -> (Integer, Integer, Char) -> (Integer, Integer, Char)
decodeRules3 input = helper $ map rules $ decodeRules input

helper :: [t -> t] -> t -> t
helper [] input = input
helper (f:fs) input = helper fs (f input)


-- >>> decodeRules smallInput
-- [('F',10),('N',3),('F',7),('R',90),('F',11)]

-- >>> rules 'S' 7 start
-- (0,-7,'E')

-- >>> start & rules 'F' 10 & rules 'N' 3 & rules 'F' 7 & rules 'R' 90 & rules 'F' 11 & displacement
-- 25

-- >>> start & decodeRules3 smallInput & displacement
-- 25

-- >>>  x <- readFile "/Users/lyndon/code/advent2020/advent12.input"
-- >>>  start & decodeRules2 x & displacement
-- 1533

