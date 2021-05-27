module Module_1618291832_32008 where

import Control.Concurrent.STM
    ( STM, atomically, newTVarIO, readTVar, TVar, modifyTVar )


tprint :: Show a => STM a -> IO a
tprint x = do
    x' <- atomically x
    print x'
    return x'

main :: IO String
main = do
    v <- newTVarIO 'x'
    let v' = readTVar v
    let g = fmap show v'
    atomically $ modifyTVar v succ
    res <- tprint g
    return res


-- >>> main
-- "'y'"


