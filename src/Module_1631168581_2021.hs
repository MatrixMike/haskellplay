module Module_1631168581_2021 where

type I2 = (Int,Int)
foo :: I2
foo = (1,2)

replaceFst :: I2 -> Int -> I2
replaceFst (_a, b) c = (c, b)

swapI2 :: I2 -> I2
swapI2 (a,b) = (b,a)

replaceElement :: [a] -> Int -> a -> [a]
replaceElement [] _ _ = []
replaceElement (_:xs) 0 a = a : xs
replaceElement (x:xs) n a = x : replaceElement xs (pred n) a

l = [1..1000000]

l' = replaceElement l 999999 7

swapAdjacentElements :: ()
swapAdjacentElements = undefined 

data Pointer a = P
    { previous :: [a]
    , current  :: a
    , next     :: [a]
    }

mkPointer :: [a] -> Maybe (Pointer a)
mkPointer [] = Nothing
mkPointer (x:xs) = Just $ P [] x xs

moveForward :: Pointer a -> Maybe (Pointer a)
moveForward = undefined

moveBack :: Pointer a -> Maybe (Pointer a)
moveBack = undefined

offset :: Int -> Pointer a -> Maybe (Pointer a)
offset = undefined

deleteElement :: Pointer a -> Maybe (Pointer a)
deleteElement = undefined 

addElement :: a -> Pointer a -> Pointer a
addElement = undefined 

swapElements :: Pointer a -> Pointer a
swapElements = undefined 





