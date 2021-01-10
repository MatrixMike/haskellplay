
module Module_1609992141_20332 where

import Network.URI
import Data.Maybe (catMaybes)
import Data.Map as M

matchURI :: String -> String -> Either String [(String,String)]
matchURI ts us = do
    t <- x ts
    u <- x us
    matchTemplate t u
    where
    x a = maybe (Left $ "Invalid Path " <> a) Right $ pathSegments <$> parseURIReference a

matchTemplate :: [String] -> [String] -> Either String [(String,String)]
matchTemplate ks vs = (sequence . catMaybes) =<< zipWithIsosceles "Mismatched template/url lengths" match ks vs

-- | Version of zipwith that requires lists to be of the same length or returns an error
zipWithIsosceles :: a -> (b -> c -> d) -> [b] -> [c] -> Either a [d]
zipWithIsosceles _ _ [] []         = Right []
zipWithIsosceles e f (l:ls) (m:ms) = (f l m :) <$> zipWithIsosceles e f ls ms
zipWithIsosceles e _ _ _           = Left e

match :: [Char] -> [Char] -> Maybe (Either String ([Char], [Char]))
match (':':k) v = Just (Right (k,v))
match k v
    | k == v    = Nothing
    | otherwise = Just (Left $ k <> " doesn't match " <> v)

insertWithA :: (Ord k, Applicative f) => (t -> t -> f t) -> k -> t -> Map k t -> f (Map k t)
insertWithA f k v m =
    case M.lookup k m of
        Nothing -> pure $ M.insert k v m
        Just v' -> flip (M.insert k) m <$> f v v'

-- >>> pathSegments <$> parseURIReference "/lol"
-- Just ["lol"]

-- >>> pathSegments <$> parseURIReference "/lol"
-- Just ["lol"]

-- >>> matchURI "~%" "..."
-- Left "Invalid Path ~%"

-- >>> matchURI "/asdf/qwer" "/asdf"
-- Left "Length of URI path segments doesn't match template"

-- >>> matchURI "/asdf/qwer" "/asdf/qwer"
-- Right []

-- >>> matchURI "/:asdf/qwer" "/asdf/qwer"
-- Right [("asdf","asdf")]

-- >>> matchURI "/zsdf/qwer" "/asdf/qwer"
-- Left "zsdf doesn't match asdf"

-- >>> matchTemplate (words "a b :c :d e :f") (words "a b c d e f")
-- Right [("c","c"),("d","d"),("f","f")]
