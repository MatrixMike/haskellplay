{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Module_1601442365_18499 where

import Data.List (tails)
import Control.Concurrent hiding (yield)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM hiding (retry)
import Data.Align
import Control.Monad
import Data.These
import Control.Concurrent.STM.TQueue

import qualified Data.Map as Map
import Conduit
import qualified Data.Conduit.Combinators as C
import Data.Foldable (for_)
import Control.Monad.Trans.Resource (allocate)
import Control.Concurrent.Async (Async, cancel, async)
import GHC.Natural (Natural)
import Data.Time (NominalDiffTime)
import Control.Exception (ErrorCall, SomeException, catch, IOException, try)


{-

Pseudo pipeline:

Subscription ( callback )
    -> Diff previous verion
    -> Update current versions
    -> Fetch changed configs (require refetch on error)
    -> Change state of servers
    -> Register state of servers

-}

main :: IO ()
main = do
    runConduitRes do
        callbackSource subscribe
            .| delay 1000000
            .| changes mempty
            .| do
                g <- liftIO newRegistry
                C.mapM_
                    (   liftIO . react
                    >=> mapM_ (\i -> liftIO $ register g i $ spawn i)
                    )

subscribe :: Monad m => (Map.Map Integer Integer -> m b) -> m ()
subscribe f = mapM_ f maps
    where
    maps    = map Map.fromList $ zipWith zip updates updates
    updates = map (take 5) (tails [0 :: Integer ..])

react :: These Integer Integer -> IO (Maybe Integer)
react x = do
    print x
    case x of
        (This  x')               -> Just <$> removed    x'
        (That  y')               -> Just <$> introduced y'
        (These x' y') | x' == y' -> Just <$> changed    x' y'
        (These x' y')            ->          unchanged  x' y'

    where

    removed    _x    = error "removed is undefined"
    introduced     y = retry fetchRetries do fetch y
    changed    _x _y = error "changed is undefined"
    unchanged  _x _y = return Nothing

retry :: m ~ IO => Natural -> m Integer -> m Integer
retry 0 _a = return (-1)
retry n  a = catch @ErrorCall a \e -> print e >> retry (pred n) a


fetch :: (Show b, Integral b) => b -> IO b
fetch x = do
    when (x `mod` 7 == 3) $ error ("Oops: " ++ show x)
    threadDelay 1000000
    return x

spawn :: Integer -> Async Integer
spawn = error "spawn undefined"

-- Retry Logic

fetchRetries :: Natural
fetchRetries = 3

-- Execution Registry

type Reg = Registry Integer

data Registry a = Reg { registry :: TVar (Map.Map a (Async a)) }

newRegistry :: IO Reg
newRegistry = Reg <$> newTVarIO mempty

register :: Ord a => Registry a -> a -> Async a -> IO ()
register (Reg r) k v = atomically (modifyTVar r (Map.insert k v))

deregister, reconfigure :: a
deregister  = error "deregister is undefined"
reconfigure = error "reconfigure is undefined"

-- Conduits

changes :: (Monad m, Foldable t, Semialign t)
        => t b -> ConduitT (t b) (These b b) m ()
changes f = do
    await >>= mapM_ \t -> do
        for_ (align f t) yield
        changes t

callbackSource :: MonadResource m
               => (forall a. (o -> IO a) -> IO ()) -> ConduitT i o m ()
callbackSource f = do
    
    m <- liftIO newEmptyMVar
    allocate
        (async do f (putMVar m . Just) *> putMVar m Nothing)
        cancel
    go m

    where
    go m =
        liftIO (takeMVar m) >>= mapM_ \x -> do
            yield x
            go m

delay :: MonadIO m => Int -> ConduitT b b m ()
delay d = C.mapM (\x -> liftIO (threadDelay d) *> return x) 