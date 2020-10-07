{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

module ChanArrow where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Concurrent
import Control.Category
import Control.Concurrent.Async (wait, Async, async)
import Control.Monad (void)
import Data.Maybe (catMaybes, isJust)
import Data.Void
import Control.Applicative (Applicative(liftA2))

main :: IO ()
main = void do feedCharrow (arrA >>> arrB) [1..10] >>= print

test :: IO ()
test = do
    o <- withCharrow (arr succ >>> arrA) \i -> do
        i 1
        i 2
    print o

test2 :: IO ()
test2 = do
    o <- withCharrow ax \i -> do
        i (1 :: Int)
        i 2
    print o
    where
        ax = proc x -> do
            y <- arr succ -< x
            id -< (x + y)

arrA :: Charrow Int String
arrA = fmap show id

arrB :: Charrow String ()
arrB = process print

-- Data

data Charrow i o = Charrow { runCharrow :: IO (Chan (Maybe i), Chan (Maybe o), Async ()) }

-- Top level

run :: Charrow i o -> IO (Async ())
run c = do
    (i,_o,a) <- runCharrow c
    writeChan i Nothing
    return a

source :: ((o -> IO ()) -> IO a2) -> Charrow Void o
source m = Charrow do
    i <- newChan
    o <- newChan
    a <- async do
        m (writeChan o . Just)
        writeChan o Nothing
    return (i,o,a)

withCharrow :: Charrow i o -> ((i -> IO ()) -> IO ()) -> IO [o]
withCharrow c cb = do
    (i,o,a) <- runCharrow c
    cb (writeChan i . Just)
    writeChan i Nothing
    c2l o

feedCharrow :: Charrow i o -> [i] -> IO [o]
feedCharrow c l = do
    (i, o, a) <- runCharrow c
    async do l2c i l >> writeChan i Nothing
    c2l o

process :: (a -> IO b) -> Charrow a b
process f = Charrow do
    i <- newChan
    o <- newChan
    a <- async do
        cs <- getChanContents i
        flip mapM_ cs \x -> do
            case x of
                Nothing -> writeChan o Nothing
                Just x' -> f x' >>= writeChan o . Just
    return (i,o,a)

-- Instances

instance Functor (Charrow a) where
    fmap f c = Charrow do
        (i,o,a) <- runCharrow c
        o' <- newChan
        a' <- async do
            getChanContents o >>= writeList2Chan o' . fmap (fmap f)
            wait a
        return (i,o',a')

instance Category Charrow where
    id    = Charrow $ async (return ()) >>= \a -> newChan >>= \c -> return (c,c,a)
    g . f = Charrow do
        (fi, fo, fa) <- runCharrow f
        (gi, go, ga) <- runCharrow g

        a <- async do
            getChanContents fo >>= writeList2Chan gi
            wait fa
            wait ga

        return (fi, go, a)

instance Arrow Charrow where
    arr = flip fmap id

    first c = Charrow do
        (i,o,a) <- runCharrow c
        i'      <- newChan
        o'      <- newChan
        is      <- getChanContents i'
        os      <- getChanContents o

        a' <- async do writeList2Chan i (map (fmap fst) is)

        a'' <- async do
            writeList2Chan o' (zipWith (liftA2 (,)) os (map (fmap snd) is))
            wait a
            wait a'

        return (i',o',a'')

-- Helpers

l2c :: Chan (Maybe a) -> [a] -> IO ()
l2c c = writeList2Chan c . fmap Just

c2l :: Chan (Maybe a) -> IO [a]
c2l c = catMaybes . takeWhile isJust <$> getChanContents c
