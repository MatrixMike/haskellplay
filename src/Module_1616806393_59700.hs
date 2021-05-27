{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Module_1616806393_59700 where

import Data.SemVer ( _Textual, fromText, release )
import Control.Lens ( Const(Const), (^?), (^.), Getting )
import qualified Data.Text as T
import Data.Monoid ( First(First) )
import Data.Coerce ( coerce )
import Control.Monad (join, replicateM_)
import GHC.Conc (getAllocationCounter)

(^??) :: forall s a b t. s -> ((a -> Const (First a) b) -> s -> Const (First a) t) -> Maybe a
(^??) = coerce ((^?) :: s -> Getting (First a) s a -> Maybe a)

main :: IO (Either String (Maybe T.Text))
main = do
    let res = do
                semver <- fromText "1.2.3-foo"
                let releaseIDs = semver ^. release
                let rid        = head releaseIDs
                return $ rid ^?? _Textual

    print res
    return res


-- >>> main
-- Right (Just "foo")


prerun :: (IO a -> IO (IO b)) -> IO (IO a -> IO b)
prerun f = return $ \ioa -> join (f ioa)

greetAndReturnPerformThrice :: Show a => IO a -> IO (IO ())
greetAndReturnPerformThrice a = do
    putStrLn "greetings"
    return . replicateM_ 3 $ a >>= print

example :: IO ()
example = do
    performThrice <- prerun greetAndReturnPerformThrice  -- [1]
    performThrice getAllocationCounter                   -- [2]
    performThrice $ pure 42                              -- [3]

-- >>> example
-- ()
