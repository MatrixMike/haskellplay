
module Module_1613472351_24871 where

import Control.Concurrent.STM

-- >>> main
-- (2,2,3,1,1)

main = do
    t <- newTChanIO 

    atomically $ writeTChan t 1

    a <- atomically $ dupTChan t
    b <- atomically $ dupTChan t
    c <- atomically $ cloneTChan t

    atomically $ writeTChan t (2 :: Int)

    atomically $ writeTChan b 3

    a' <- atomically $ readTChan a
    b' <- atomically $ readTChan b
    b'' <- atomically $ readTChan b
    c' <- atomically $ readTChan c

    t' <- atomically $ readTChan t

    return (a', b', b'', c', t')
