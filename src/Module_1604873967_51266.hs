
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE InstanceSigs #-}

import Control.Applicative (Alternative(..))
import Control.Monad ()

data ResultIO e a = ResultIO { unResultIO :: IO (Either e a) }

pattern Wrong :: a -> Either a b
pattern Wrong x = Left x
pattern Correct :: b -> Either a b
pattern Correct x = Right x

instance Functor (ResultIO e) where

instance Applicative (ResultIO e) where
  pure x = ResultIO do pure (Correct x)
  (ResultIO f) <*> (ResultIO x) = ResultIO do
    v1 <- f
    v2 <- x
    case v1 of
      Wrong v -> pure $ Wrong v
      Correct fn -> case v2 of
        Wrong j -> pure $ Wrong j
        Correct d -> pure $ Correct (fn d)

instance Monoid e => Alternative (ResultIO e) where
  empty = ResultIO do pure (Wrong mempty)
  a <|> b = ResultIO do
    a' <- unResultIO a
    case a' of
      Correct a'' -> pure (Correct a'')
      Wrong a'' -> do
        b' <- unResultIO b
        case b' of
          Wrong b'' -> pure (Wrong (a'' <> b''))
          Correct b'' -> pure (Correct b'')



-- instance Applicative (ResultIO e) where
--   (ResultIO f) <*> (ResultIO x) = do
--     v1 <- f
--     v2 <- x
--     case v1 of
--       Wrong v -> ResultIO $ pure $ Wrong v
--       Correct fn -> case v2 of
--         Wrong j -> ResultIO $ pure $ Wrong j
--         Correct d -> ResultIO $ pure $ Correct (fn d)


newtype HasIO a b = HasIO { runHasIO :: Has a (IO b) } 

newtype Has a b = Has { runHas :: a -> b }

-- instance Monad (HasIO c) where
--   (>>=) :: HasIO c a -> (a -> HasIO c b) -> HasIO c b
--   HasIO (Has h) >>= f =
--     HasIO $ Has $ \c -> do
--       let ioa = h c
--       a <- ioa
--       let HasIO (Has iob) = f a
--       iob c



