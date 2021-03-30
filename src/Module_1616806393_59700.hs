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
