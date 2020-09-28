{-# LANGUAGE BlockArguments #-}

module ConduitResourceExperiment where

import Data.Conduit
import Control.Monad
import Control.Concurrent hiding (yield)
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource

main :: IO ()
main = runConduitRes do a .| b

-- >>> main
-- Just 1
-- cancelling

a :: MonadResource m => ConduitT i Int m b
a = do
    m <- liftIO newEmptyMVar
    allocate
        (async do putMVar m 1 >> putMVar m 2 >> putMVar m 3)
        (\x -> putStrLn "cancelling" >> cancel x)

    forever do
        x <- liftIO do takeMVar m
        yield x

b :: (Monad m, MonadIO m) => ConduitT Int o m ()
b = do
    x <- await
    liftIO do print x
