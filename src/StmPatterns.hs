{-# language ApplicativeDo #-}
{-# language BlockArguments #-}
{-# language OverloadedStrings #-}

module Main where

import Control.Concurrent.STM
import Control.Concurrent.STM.TVar


-- App

import Web.Scotty
import Web.Scotty.Internal.Types

import Data.Time.Clock ( getCurrentTime, UTCTime )
import Data.Text.Lazy
import Control.Monad.IO.Class
import Control.Concurrent.Async
import Control.Monad (forever, join, when)
import Control.Concurrent (threadDelay)
import Control.Monad.Loops (iterateM_)


-- Ideas for server:
-- Quantized time server?

showt :: Show x => x -> Text
showt = pack . show


-- main :: IO ()
-- main = do
--     scotty 8888 do
--         get "/time" do
--             t <- liftIO getCurrentTime
--             (html $ " <b>" <> showt t <> "</b>")



-- Broke:

-- Manual data passing

data Config = Config
    { time :: UTCTime
    , foo  :: String
    , bar  :: Text
    }
    deriving Eq

getSpecialString :: Config -> IO String
getSpecialString _c = do
    print "lol"
    pure "asdf"

-- Concerns:
-- * Slow
-- * Impure
-- * Expensive

getConfig :: IO Config
getConfig = do
    t <- getCurrentTime
    pure $ Config  t "hello" "world"

-- main :: IO ()
-- main = do
--     forever do
--         threadDelay 1000000
--         t <- async do
--             c <- liftIO getConfig
--             scotty 8888 do
--                 get "/time" do
--                     (html $ " <b>" <> showt (time c) <> "</b>")
--         threadDelay 1000000
--         cancel t


-- Woke:

-- Dynamic TVar

-- main :: IO ()
-- main = do
--     c <- newTVarIO =<< getConfig

--     async do
--         forever do
--             threadDelay 1000000
--             c' <- getConfig
--             atomically do writeTVar c c'

--     server c

-- server :: TVar Config -> IO ()
-- server c = do
--     c' <- liftIO $ readTVarIO c
--     t <- liftIO $ newTVarIO (time c')
--     async do
--         forever do
--             threadDelay 1000000
--             c' <- liftIO $ readTVarIO c
--             liftIO $ atomically $ writeTVar t (time c')
--     scotty 8888 do route t

-- route :: TVar UTCTime -> ScottyM ()
-- route t = get "/time" do myHandler t

-- myHandler :: TVar UTCTime -> ActionT Text IO ()
-- myHandler t = do
--             t' <- liftIO $ readTVarIO t
--             (html $ " <b>" <> showt t' <> "</b>")


-- Bespoke:

main :: IO ()
main = do
    c <- newTVarIO =<< getConfig

    async do
        forever do
            threadDelay 1000000
            c' <- getConfig
            atomically do writeTVar c c'

    server (readTVar c)

getNew :: Eq a => a -> STM a -> STM a
getNew x s = do
    x' <- s
    check (x /= x')
    return x'

(<$$>) :: Eq a => (a -> IO b) -> STM a -> IO (STM b)
f <$$> x = do
    x' <- atomically x
    y <- f x'
    t <- newTVarIO y
    async do 
        flip iterateM_ x' \p -> do
            x'' <- atomically $ getNew p x
            when (p /= x'') do
                y' <- f x''
                atomically $ writeTVar t y'
            pure x''
    pure (readTVar t)

server :: STM Config -> IO ()
server c = do
    let t = time <$> c
    let x = bar <$> c
    ss <- getSpecialString <$$> c
    scotty 8888 do route x t ss

route :: STM Text -> STM UTCTime -> STM String -> ScottyM ()
route x t ss = get "/time" do myHandler x t ss

myHandler :: STM Text -> STM UTCTime -> STM String -> ActionT Text IO ()
myHandler x t ss = do
            ss' <- liftIO $ atomically ss
            (t', x') <- liftIO $ atomically do (,) <$> t <*> x
            (html $ " <b> " <> pack ss' <> " ~~~~ " <> x' <> " ~~~ " <> showt t' <> "</b>")

-- STM

-- Approaches to Effectful sub config construction

-- 1. Defer the effect!
-- 1b. Debounce, cache, etc.
-- 2. fmapM
-- 3. Push computation to dedicated region ... upstream?



