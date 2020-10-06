{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module ChanArrow where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Concurrent
import Control.Category
import Control.Concurrent.Async (async)
import Control.Monad (void)
import Data.Maybe (catMaybes, isJust)

main :: IO ()
main = void do feedCharrow (a >>> b) [1..10] >>= print

data Charrow i o = Charrow { runCharrow :: IO (Chan (Maybe i), Chan (Maybe o)) }

l2c :: Chan (Maybe a) -> [a] -> IO ()
l2c c = writeList2Chan c . fmap Just

c2l :: Chan (Maybe a) -> IO [a]
c2l c = catMaybes . takeWhile isJust <$> getChanContents c

instance Functor (Charrow a) where
    fmap f c = Charrow do
        (i,o) <- runCharrow c
        o' <- newChan
        async do getChanContents o >>= writeList2Chan o' . fmap (fmap f)
        return (i,o')

instance Category Charrow where
    id    = Charrow $ newChan >>= \c -> return (c,c)
    g . f = Charrow do
        (fi, fo) <- runCharrow f
        (gi, go) <- runCharrow g

        async do getChanContents fo >>= writeList2Chan gi
        return (fi, go)

feedCharrow :: Charrow i o -> [i] -> IO [o]
feedCharrow c i = do
    (fi, fo) <- runCharrow c
    async do l2c fi i >> writeChan fi Nothing
    c2l fo

a :: Charrow Int String
a = fmap show id

process :: (a -> IO b) -> Charrow a b
process f = Charrow do
    i <- newChan
    o <- newChan
    async do
        cs <- getChanContents i
        flip mapM_ cs \x -> do
            case x of
                Nothing -> writeChan o Nothing
                Just x' -> f x' >>= writeChan o . Just
    return (i,o)

b :: Charrow String ()
b = process print
