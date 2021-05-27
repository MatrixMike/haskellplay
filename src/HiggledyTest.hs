{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

module HiggledyTest where

import Data.Aeson as Aeson
import Control.Concurrent.STM
import GHC.Generics

import Data.Generic.HKD

import qualified Data.ByteString.Lazy.Char8 as BS

data ServeOptions = ServeOptions
    { adminSecret   :: String
    , enableConsole :: Bool
    }
    deriving Generic

type ServeOptionsF f = HKD ServeOptions f
type Mutable       a = HKD a TVar
type ServeOptionsM   = Mutable ServeOptions

instance ToJSON ServeOptions where

mkMutable :: ServeOptions -> ServeOptionsM
mkMutable = undefined -- deconstruct

mkPure :: ServeOptionsM -> STM ServeOptions
mkPure = undefined -- construct

runServer :: ServeOptionsM -> IO ()
runServer = undefined

printAeson :: Aeson.ToJSON a => a -> IO ()
printAeson = BS.putStrLn . Aeson.encode

main :: IO ()
main = do
    {-
    opts <- mkOptions
    thread <- async $ runServer opts
    putStrLn . Aeson. =<< Aeson.encode (mkPure opts))
    disableConsole opts
    putStrLn (Aeson.encode (mkPure opts))

    config     <- getInitialConfig
    configM    <- mkPartiallyMutable config
    thread     <- startServer configM
    newConfig  <- getInitialConfig
    if onlyMutableFieldsHaveChanged config configM newConfig
    then mutateFields configM newConfig
    else restartServer thread

    -}
    print "hello world"

