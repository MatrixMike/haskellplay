{-# LANGUAGE BlockArguments #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE GADTs #-}
module Module_1619505011_11358 where

import           Control.Concurrent.Async (async)
import           Control.Concurrent.STM   (STM, atomically, check, newTVarIO, readTVar, writeTVar)
import           Control.Monad            ((<=<))

waitNewValue :: Eq a => a -> STM a -> STM a
waitNewValue a s = do
    s' <- s
    check (a /= s')
    return s'

loopNewValues :: (m ~ IO, Eq a) => (a -> m b) -> STM a -> a -> m ()
loopNewValues f s v = do
    v' <- atomically do waitNewValue v s
    f v'
    loopNewValues f s v'

(<$$>) :: (m ~ IO, Eq a) => (a -> m b) -> STM a -> m (STM b)
f <$$> s = do
    v <- atomically do s
    v' <- f v
    t <- newTVarIO v'
    async do loopNewValues (atomically . writeTVar t <=< f) s v -- Would be great to do this without a new thread.
    pure do readTVar t

-- >>> atomically do waitNewValue (5 :: Int) (pure 5)
-- ProgressCancelledException
