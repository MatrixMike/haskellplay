module Module_1599437482_67587 where

import Data.List
import Data.Void
import Data.Monoid
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace _ _ [] = []
replace f t s@(h:hs)
  | isPrefixOf f s = t ++ replace f t (drop (length f) s)
  | otherwise      = h : replace f t hs

fixpoint :: Eq b => (b -> b) -> b -> b
fixpoint f x =
  let
    i = iterate f x
    z = zip i (tail i)
    d = dropWhile (uncurry (/=)) z
  in
    snd $ head d

balanced :: String -> Bool
balanced = null . fixpoint (replace "()" "")

main :: IO ()
main = do
  print $ balanced "())()()(((("
  parseTest (psr 0) "())()()(((("

pairs :: [(Char, Char)]
pairs = map (\[a,b] -> (a,b)) $ words "() {} []"

psr :: Sum Int -> Parsec Void String (Sum Int)
psr i = do
  x <- choice $ map (\(x,y) -> between (char x) (char y) (psr (succ <$> i) <|> return i)) pairs
  y <- try (psr i) <|> return i
  return $ max x y
