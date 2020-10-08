{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Arrows #-}

-- | CSP like library: Arrow interface
--
-- TODO:
-- 
--   * [x] Recovery/Retry capability
--   * [x] Fix await deadlock
--   * [x] Generic Chan functions, then specific newtype
--   * [x] Different transport options, buffered, etc.
--   * [ ] Different transports for sections of the graph
--   * [ ] Allow configurable parallelism
--   * [ ] Early termination if downstream consumer completes
-- 
module Churro where

import Prelude hiding (id, (.))
import Control.Arrow
import Control.Concurrent
import Control.Category
import Control.Concurrent.Async (cancel, wait, Async, async)
import Data.Maybe (isNothing, catMaybes)
import Data.Void
import Control.Applicative (Applicative(liftA2))
import Data.Foldable (for_)
import Control.Exception (Exception, SomeException, try)
import GHC.Natural (Natural)
import System.Random
import Control.Monad (replicateM)
import Data.Time (NominalDiffTime)
import Data.Map (fromList)
import Data.List (tails)

-- Tests

main :: IO ()
main = do
    runWait @Chan $ sourceList [1::Int ..10] >>> delay 0.3 >>> sinkPrint
    runWait @Chan test0
    runWait @Chan test1
    runWait @Chan test2
    runWait @Chan test3
    runWait @Chan test4
    runWait @Chan test5
    runWait @Chan (id >>> id)
    runWait @Chan id
    runWait @Chan $ sourceList [1 :: Int ..5] >>> withPrevious >>> sinkPrint
    runWait @Chan ( sourceIO (\cb -> replicateM 3 do cb (5 :: Int))
        >>> delay 1
        >>> arr show
        >>> sinkPrint
        )

    runWait @Chan $ sourceList [1::Int ..] >>> takeC (2::Int) >>> sinkPrint
    runWait pipeline
    return ()

test0 :: Transport t => Churro t Void Void
test0 = fmap id id

test1 :: Transport t => Churro t Void Void
test1 = sourceList [1::Int,2] >>> arr succ >>> arr show >>> sinkIO putStrLn

test2 :: Transport t => Churro t Void Void
test2 = sourceList [1::Int ..11] >>> prog >>> sinkPrint
    where
    prog = proc i -> do
        j <- arr succ  -< i
        k <- arr show  -< j
        l <- arr succ  -< j
        m <- arr (> 5) -< j
        n <- process (\x@(_x,_y,z) -> print x >> return z) -< (k,l,m)

        arr not -< n

test3 :: Transport t => Churro t Void Void
test3 = sourceList [1 :: Int] >>> process print >>> sinkPrint

test4 :: Transport t => Churro t Void Void
test4 = sourceList [1 :: Int] >>> (process print &&& process print) >>> sinkPrint

test5 :: Transport t => Churro t Void Void
test5 = sourceList [1 :: Int ..10] >>> arr (0 :: Natural,) >>> processRetry' @SomeException 20 flakeyThing >>> sinkPrint
    where
    flakeyThing x = do
        r <- randomRIO (1::Int,10)
        if x < 3 || r > 5
            then return x
            else error ("oops! " <> show x)

type ChurroChan = Churro Chan

pipeline :: ChurroChan Void Void
pipeline = sourceList maps >>> takeC (10 :: Int) >>> delay 0.5 >>> withPrevious >>> sinkPrint
    where
    maps    = map fromList $ zipWith zip updates updates
    updates = map (take 5) (tails [0 :: Int ..])

-- Runners

runWait :: Transport t => Churro t Void Void -> IO ()
runWait x = wait =<< run x

run :: Transport t => Churro t Void Void -> IO (Async ())
run = run'

-- | This is unsafe, since the pipeline may not be terminated
run' :: Transport t => Churro t i o -> IO (Async ())
run' c = do
    -- Compose an empty sourceList to ensure termination
    (_i,_o,a) <- runChurro (sourceList [] >>> c)
    return a

-- Library

buildChurro :: Transport t => (t (Maybe i) -> t (Maybe o) -> IO ()) -> Churro t i o
buildChurro cb = Churro do
    i <- flex
    o <- flex
    a <- async do cb i o
    return (i,o,a)

sourceList :: (Transport t, Foldable f) => f o -> Churro t Void o
sourceList = sourceIO . for_

sourceIO :: Transport t => ((o -> IO ()) -> IO a2) -> Churro t Void o
sourceIO cb =
    buildChurro \_i o -> do
        cb (yeet o . Just)
        yeet o Nothing

sinkIO :: Transport t => (o -> IO ()) -> Churro t o Void
sinkIO cb = buildChurro \i _o -> mapM_ cb =<< c2l i

sinkPrint :: (Transport t, Show a) => Churro t a Void
sinkPrint = sinkIO print

process :: Transport t => (a -> IO b) -> Churro t a b
process f = processN (fmap pure . f)

processN :: Transport t => (a -> IO [b]) -> Churro t a b
processN f =
    buildChurro \i o -> do
        cs <- c2l i
        for_ cs \x -> mapM_ (yeet o . Just) =<< f x
        yeet o Nothing

justs :: Transport t => Churro t (Maybe a) a
justs = mapN (maybe [] pure)

lefts :: Transport t => Churro t (Either a b) a
lefts = mapN (either pure (const []))

rights :: Transport t => Churro t (Either a b) b
rights = mapN (either (const []) pure)

takeC :: (Transport t, Integral n) => n -> Churro t a a
takeC n = buildChurro \i o -> do
    l <- replicateM (fromIntegral n) (yank i)
    yeetList o l
    yeet o Nothing

mapN :: Transport t => (a -> [b]) -> Churro t a b
mapN f = processN (return . f)

delay :: Transport t => NominalDiffTime -> Churro t a a
delay = delayMicro . ceiling @Double . fromRational . (*100000) . toRational

delayMicro :: Transport t => Int -> Churro t a a
delayMicro d = process \x -> do
    threadDelay d
    return x

withPrevious :: Transport t => Churro t a (a,a)
withPrevious = buildChurro \i o -> do
    l <- c2l i
    yeetList o (fmap Just $ zip l (tail l))
    yeet o Nothing

-- | Requeue an item if it fails.
--   Note: There is an edgecase with Chan transport where a queued retry may not execute
--         if a source completes and finalises before the item is requeued.
processRetry :: Transport t => Natural -> (i -> IO o) -> Churro t i o
processRetry maxRetries f = arr (0,) >>> processRetry' @SomeException maxRetries f >>> rights

processRetry' :: (Exception e, Transport t) => Natural -> (a -> IO o) -> Churro t (Natural, a) (Either e o)
processRetry' maxRetries f =
    buildChurro \i o -> do
        cs <- c2l' i
        for_ cs \x -> do
            -- TODO: Figure out how to eliminate the case naturally
            case x of
                Nothing -> yeet o Nothing
                Just (n, y) -> do
                    r <- try do f y
                    case r of
                        Right _ -> yeet o (Just r)
                        Left  err
                            | n >= maxRetries -> return ()
                            | otherwise       -> do
                                yeet i (Just (succ n, y))
                                yeet o (Just (Left err))

-- Data, Classes and Instances

data Churro t i o = Churro { runChurro :: IO (t (Maybe i), t (Maybe o), Async ()) }

class Transport t where
    flex     :: IO (t a)
    yank     :: t a -> IO a
    yeet     :: t a -> a -> IO ()
    yankList :: t a -> IO [a]

yeetList :: (Foldable t1, Transport t2) => t2 a -> t1 a -> IO ()
yeetList t = mapM_ (yeet t)

instance Transport Chan where
    flex = newChan
    yank = readChan
    yeet = writeChan
    yankList = getChanContents

instance Transport t => Functor (Churro t a) where
    fmap f c = Churro do
        (i,o,a) <- runChurro c
        o'  <- flex
        a'  <- async do
            c2c f o o'
            wait a
        return (i,o',a')

instance Transport t => Category (Churro t) where
    id    = Churro do async (return ()) >>= \a -> flex >>= \c -> return (c,c,a)
    g . f = Churro do
        (fi, fo, fa) <- runChurro f
        (gi, go, ga) <- runChurro g

        a <- async do
            c2c id fo gi
            wait ga
            cancel fa
            yeet gi Nothing
            yeet fi Nothing

        return (fi, go, a)

instance Transport t => Arrow (Churro t) where
    arr = flip fmap id

    first c = Churro do
        (i,o,a) <- runChurro c
        i'      <- flex
        o'      <- flex
        is      <- c2l' i'
        os      <- c2l' o

        a'   <- async do yeetList i (map (fmap fst) is)
        a''  <- async do yeetList o' (zipWith (liftA2 (,)) os (map (fmap snd) is))
        a''' <- async do
            wait a
            wait a'
            wait a''

        return (i',o',a''')

-- Transport Helpers

l2c :: Transport t => t (Maybe a) -> [a] -> IO ()
l2c c = yeetList c . fmap Just

c2l :: Transport t => t (Maybe a) -> IO [a]
c2l = fmap catMaybes . c2l'

c2l' :: Transport t => t (Maybe a) -> IO [Maybe a]
c2l' c = takeUntil isNothing <$> yankList c

c2c :: Transport t => (a1 -> a2) -> t (Maybe a1) -> t (Maybe a2) -> IO (Async ())
c2c f i o = do
    l <- c2l i
    async do
        l2c o (fmap f l)
        yeet o Nothing

-- Other Helpers

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ []     = []
takeUntil p (x:xs)
    | p x       = [x]
    | otherwise = x : takeUntil p xs

