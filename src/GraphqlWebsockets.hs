{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

-- | Small demonstration of GraphQL subscriptions over websockets as per https://github.com/apollographql
-- 
-- * Intended to be a convention over configuration approach.
-- * Retries subscription on failures.
-- * Provides callbacks for success and errors.
--
-- TODO:
-- 
-- * [ ] Support secure sockets
-- * [ ] Support recieving entire response
-- * [ ] Allow passing options to client
-- * [ ] Allow passing options to request
-- * [ ] Allow configuring retry interval
-- * [ ] Allow multiplexing requests over connection
-- * [ ] Allow terminating subscription somehow
-- * [ ] Allow using a URL instead of host, port, endpoint
-- * [ ] Tidy up ClientError type
-- * [ ] Create datatype to better represent GraphQL requests
-- * [ ] Create convenience quasiquoter
-- * [ ] Create mocking capability
-- * [ ] Create tests

module GraphqlWebsockets where

import           Control.Arrow
import           Control.Concurrent
import           Control.Exception
import           Control.Lens       (traverseOf, traverseOf_, traversed, (^?))
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens
import           Data.ByteString
import           Data.Foldable      (traverse_)
import           Data.Text          hiding (null)
import           Network.WebSockets

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text.IO               as T

main :: IO ()
main = runClientImmortal "localhost" 8888 "/v1/graphql" print (subscribe query rescue callback)
    where

    -- Uses the chinook schema for testing: https://github.com/lerocha/chinook-database#chinook-database

    query :: Value
    query =
        object
            [ "variables" .= object ["limit" .= Number 11]
            , "query"     .= String "subscription S($limit: Int) { artists(limit: $limit, where: {id: {_eq: 1}}) { name albums { id } id }}" ]

    callback :: Value -> IO ()
    callback v = do
        Prelude.putStrLn "\n\nUpdated Artists:\n"
        traverseOf_ (key "artists" . _Array . traverse) (B.putStrLn . encode) v

    rescue :: Value -> IO ()
    rescue = B.putStrLn . encode

-- Recovers from typical exceptions

type ClientError = Either HandshakeException (Either IOException (Either ConnectionException ()))

runClientImmortal :: String -> Int -> String -> (ClientError -> IO ()) -> ClientApp () -> IO b
runClientImmortal host port endpoint rescue callback = do
    forever do
        outcome <- try @HandshakeException
                 $ try @IOException
                 $ try @ConnectionException
                 $ runClient host port endpoint callback
        print outcome
        case outcome of
            Right (Right (Right ())) -> return ()
            ex                       -> rescue ex >> threadDelay 1000000

subscribe :: (FromJSON a, ToJSON a, ToJSON v) => v -> (Value -> IO ()) -> (a -> IO ()) -> ClientApp ()
subscribe payload rescue callback connection = do

    -- The lazy option determines when to establish connection:
    --   https://github.com/apollographql/subscriptions-transport-ws

    let establish = object [ t "lazy" "false", "headers" .= object [ t "content-type" "application/json" ] ]

    subscribe' establish payload connection \(message :: B.ByteString) -> do
        case eitherDecode @Value message of
            Left e -> throwIO $ userError e -- IOException
            Right response -> do
                traverse_ callback $ response ^? key "payload" . key "data" . _JSON
                traverse_ rescue   $ response ^? key "payload" . key "errors" . _Array . traverse

subscribe' :: (WebSocketsData t, ToJSON v1, ToJSON v2) => v2 -> v1 -> Connection -> (t -> IO a) -> IO b
subscribe' establishPayload queryPayload connection callback = do

    let
        establish = object [ t "type" "connection_init", "payload" .= establishPayload ]
        query     = object [ t "type" "start",           "payload" .= queryPayload, t "id" "1" ]

    sendBinaryData connection (encode establish)
    sendBinaryData connection (encode query)

    forever do receiveData connection >>= callback

t :: KeyValue kv => Text -> Text -> kv
t k v = k .= String v