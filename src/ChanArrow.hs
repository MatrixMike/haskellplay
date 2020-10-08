{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

-- | CSP like library: Arrow interface
--
-- TODO:
-- 
--   * Recovery/Retry capability
--   * Fix await deadlock
--   * Allow configurable parallelism
--   * Generic Chan functions, then specific newtype
--   * Different transport options, buffered, etc.
-- 
module ChanArrow where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Concurrent
import Control.Category
import Control.Concurrent.Async (wait, Async, async)
import Data.Maybe (isNothing, catMaybes)
import Data.Void
import Control.Applicative (Applicative(liftA2))
import Data.Foldable (for_)
import Control.Exception (Exception, SomeException, try)
import GHC.Natural (Natural)
import System.Random
import Control.Monad (replicateM)

main :: IO ()
main = do
    run test0 >>= wait
    run test1 >>= wait
    run test2 >>= wait
    run test3 >>= wait
    run test4 >>= wait
    run test5 >>= wait
    run (id >>> id) >>= wait
    run id >>= wait
    run ( sourceIO (\cb -> replicateM 3 do cb (5 :: Int) >> threadDelay 1000000)
          >>> arr show
          >>> sinkIO print
        ) >>= wait

    return ()

test0 :: Charrow Void Void
test0 = fmap id id

test1 :: Charrow Void Void
test1 = sourceList [1::Int,2] >>> arr succ >>> arr show >>> sinkIO putStrLn

test2 :: Charrow Void Void
test2 = sourceList [1::Int ..11] >>> prog >>> sinkIO print
    where
    prog = proc i -> do
        j <- arr succ  -< i
        k <- arr show  -< j
        l <- arr succ  -< j
        m <- arr (> 5) -< j
        n <- process (\x@(_x,_y,z) -> print x >> return z) -< (k,l,m)

        arr not -< n

test3 :: Charrow Void Void
test3 = sourceList [1 :: Int] >>> process print >>> sinkIO print

test4 :: Charrow Void Void
test4 = sourceList [1 :: Int] >>> (process print &&& process print) >>> sinkIO print

test5 :: Charrow Void Void
test5 = sourceList [1 :: Int ..10] >>> arr (0 :: Natural,) >>> processRetry' @SomeException 20 flakeyThing >>> sinkIO print
    where
    flakeyThing x = do
        r <- randomRIO (1::Int,10)
        if x < 3 || r > 5
            then return x
            else error ("oops! " <> show x)


-- Data

data Charrow i o = Charrow { runCharrow :: IO (Chan (Maybe i), Chan (Maybe o), Async ()) }

-- Top level

run :: Charrow Void Void -> IO (Async ())
run = run'

-- | This is unsafe, since the pipeline may not be terminated
run' :: Charrow i o -> IO (Async ())
run' c = do
    -- Compose an empty sourceList to ensure termination
    (_i,_o,a) <- runCharrow (sourceList [] >>> c)
    return a

buildCharrow :: (Chan (Maybe i) -> Chan (Maybe o) -> IO ()) -> Charrow i o
buildCharrow cb = Charrow do
    i <- newChan
    o <- newChan
    a <- async do cb i o
    return (i,o,a)

sourceList :: Foldable t => t o -> Charrow Void o
sourceList = sourceIO . for_

sourceIO :: ((o -> IO ()) -> IO a2) -> Charrow Void o
sourceIO cb =
    buildCharrow \_i o -> do
        cb (writeChan o . Just)
        writeChan o Nothing

sinkIO :: (o -> IO ()) -> Charrow o Void
sinkIO cb =
    buildCharrow \i _o -> do
        mapM_ cb =<< c2l i

process :: (a -> IO b) -> Charrow a b
process f =
    buildCharrow \i o -> do
        cs <- c2l' i
        for_ cs \x -> do
            -- TODO: Figure out how to eliminate the case naturally
            case x of
                Nothing -> writeChan o Nothing
                Just y  -> writeChan o . Just =<< f y

processN :: (a -> IO [b]) -> Charrow a b
processN f =
    buildCharrow \i o -> do
        cs <- c2l' i
        for_ cs \x -> do
            -- TODO: Figure out how to eliminate the case naturally
            case x of
                Nothing -> writeChan o Nothing
                Just y  -> mapM_ (writeChan o . Just) =<< f y

processRetry :: Natural -> (i -> IO o) -> Charrow i o
processRetry maxRetries f = arr (0,) >>> processRetry' @SomeException maxRetries f >>> rights

justs :: Charrow (Maybe a) a
justs = mapN (maybe [] pure)

lefts :: Charrow (Either a b) a
lefts = mapN (either pure (const []))

rights :: Charrow (Either a b) b
rights = mapN (either (const []) pure)

mapN :: (a -> [b]) -> Charrow a b
mapN f = processN (return . f)

-- | Requeue an item if it fails.
--   Note: There is an edgecase with Chan transport where a queued retry may not execute
--         if a source completes and finalises before the item is requeued.
processRetry' :: Exception e => Natural -> (a -> IO o) -> Charrow (Natural, a) (Either e o)
processRetry' maxRetries f =
    buildCharrow \i o -> do
        cs <- c2l' i
        for_ cs \x -> do
            -- TODO: Figure out how to eliminate the case naturally
            case x of
                Nothing -> writeChan o Nothing
                Just (n, y) -> do
                    r <- try do f y
                    case r of
                        Right _ -> writeChan o (Just r)
                        Left  err
                            | n >= maxRetries -> return ()
                            | otherwise       -> do
                                writeChan i (Just (succ n, y))
                                writeChan o (Just (Left err))

-- Instances

instance Functor (Charrow a) where
    fmap f c = Charrow do
        (i,o,a) <- runCharrow c
        o'  <- newChan
        a'  <- async do
            c2c f o o'
            wait a
        return (i,o',a')

instance Category Charrow where
    id    = Charrow do async (return ()) >>= \a -> newChan >>= \c -> return (c,c,a)
    g . f = Charrow do
        (fi, fo, fa) <- runCharrow f
        (gi, go, ga) <- runCharrow g

        a <- async do
            c2c id fo gi
            wait fa
            wait ga

        return (fi, go, a)

instance Arrow Charrow where
    arr = flip fmap id

    first c = Charrow do
        (i,o,a) <- runCharrow c
        i'      <- newChan
        o'      <- newChan
        is      <- c2l' i'
        os      <- c2l' o

        a'   <- async do writeList2Chan i (map (fmap fst) is)
        a''  <- async do writeList2Chan o' (zipWith (liftA2 (,)) os (map (fmap snd) is))
        a''' <- async do
            wait a
            wait a'
            wait a''

        return (i',o',a''')

-- Helpers

l2c :: Chan (Maybe a) -> [a] -> IO ()
l2c c = writeList2Chan c . fmap Just

c2l :: Chan (Maybe a) -> IO [a]
c2l = fmap catMaybes . c2l'

c2l' :: Chan (Maybe a) -> IO [Maybe a]
c2l' c = takeUntil isNothing <$> getChanContents c

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []     = []
takeUntil p (x:xs)
    | p x       = [x]
    | otherwise = x : takeUntil p xs

c2c :: (a1 -> a2) -> Chan (Maybe a1) -> Chan (Maybe a2) -> IO ()
c2c f i o = do
    l <- c2l i
    l2c o (fmap f l)
    writeChan o Nothing

