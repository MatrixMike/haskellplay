{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module HiggledyTest where

import Data.Aeson as Aeson
import Data.Functor.Identity
import Control.Concurrent.STM
import GHC.Generics

data ServeOptions f = ServeOptions
    { adminSecret   :: String
    , enableConsole :: !(f Bool)
    }
    deriving Generic

instance ToJSON (ServeOptions Identity) where

runServer :: (ServeOptions STM) -> IO ()
runServer = undefined



main :: IO ()
main = do
    {-
    opts <- mkOptions
    thread <- async $ runServer opts
    putStrLn (Aeson.encode (mkPure opts))
    disableConsole opts
    putStrLn (Aeson.encode (mkPure opts))
    -}
    print "hello world"

