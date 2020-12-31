
module Module_1608777079_35823 where

import Text.Parsec ( string, choice, eof, parse, try, Parsec )
import Control.Lens ( (&), (.~), Field2(_2) )
import Data.Tree ( Tree(Node), drawTree )
import Data.Maybe ( fromJust )
import Control.Arrow (second, (&&&))
import Data.Either (rights)
import Data.List.Split (splitOn)
import Data.Char (isDigit)
import qualified Data.Map as M

sub t@("8", _) = t & _2 .~ ([["42"],["42","8"]])
sub t@("11",_) = t & _2 .~ ([["42","31"],["42","11","31"]])
sub t          = t

m!k = fromJust $ M.lookup k m

build :: [(String,[[String]])] -> Parsec String () (Tree String)
build r = fmap (Node "0") $ a ! "0" <* eof
	where
	a = M.fromList (map (second g) r)
	g = choice . map (try . traverse f)
	f n@(x:xs)
		| isDigit x = Node n <$> a ! n
		| otherwise = Node ss [] <$ string ss where ss = init xs

foo =
    (\x -> map drawTree x ++ [show $ length x])
    . rights
    . (\(s,p) -> map (parse p "rules") s)
    . (last &&& build . map (sub . (init . head &&& splitOn ["|"] . tail) . words) .  head)
    . splitOn [""]

bar :: IO ()
bar = do
    i <- readFile "/Users/lyndon/code/advent2020/advent19b.input.small"
    putStrLn $ unlines $ foo (lines i)

-- >>> bar

