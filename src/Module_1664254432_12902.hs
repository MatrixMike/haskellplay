import Control.Lens
import Data.List (isInfixOf)

fibs :: Fold () Integer
fibs f () = go 1 1
    where go x y = f x *> go y (x + y)

findFibWith :: [Char] -> Maybe Integer
findFibWith substr = findOf fibs (isInfixOf substr . show) ()

partA :: Int -> [Char]
partA n =
    take 8 (show result)
    where
        firstFibs =  ()^..taking n fibs >>= show
        Just result = findFibWith firstFibs

-- >>> partA 5
-- "19164316"

partB :: Int -> [Char]
partB n =
    take 8 (show result)
    where
        Just result = findFibWith (partA n)

-- >>> partB 5
-- "19164316"
