module Module_1631774056_84672 where


fibs :: [Integer]
fibs = 1 : 1 : zipWith  (+) fibs (tail fibs)

-- >>> take 10 fibs
-- [1,1,2,3,5,8,13,21,34,55]



-- Tricks
-- Use mutable referece - Just do it like C
-- Tying the Knot
-- Directory approach
--     Graph a = Node a [Graph a]
--     Graph a = Graph [Node a] [Edge]
--     Edge  a = (String, String)
--     Node  a = (String, a)
--   First - Build the directory
--   Second - transform into a real tied knot
