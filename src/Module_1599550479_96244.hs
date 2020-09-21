{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE ApplicativeDo  #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeApplications #-}

module Module_1599550479_96244 where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class

newtype RunEverything a = RE
  { runEverything :: forall m. MonadIO m => m (Either SomeException a)
  }
  deriving Functor

instance Applicative RunEverything where
  pure x = RE (pure (Right x))
  liftA2 f (RE a) (RE b) = RE do
    a' <- a
    b' <- b
    pure (f <$> a' <*> b')

instance Monad RunEverything where
  (>>=) :: RunEverything a -> (a -> RunEverything b) -> RunEverything b
  RE a >>= f = RE do
    a' <- a
    case a' of
      Left  s   -> return $ Left s
      Right a'' -> runEverything (f a'')

instance MonadIO RunEverything where
  liftIO a = RE (liftIO $ try a)

-- Transformer

newtype RunEverythingT f a = RET
  { runEverythingT :: f (Either SomeException a) } deriving Functor

instance Applicative f => Applicative (RunEverythingT f) where
  pure x = RET (pure (Right x))
  liftA2 f (RET a) (RET b) = RET do
    a' <- a
    b' <- b
    pure (f <$> a' <*> b')

instance Monad f => Monad (RunEverythingT f) where
  RET a >>= f = RET do
    a' <- a
    case a' of
      Left s    -> pure $ Left s
      Right a'' -> runEverythingT (f a'')

instance MonadIO f => MonadIO (RunEverythingT f) where
  liftIO a = RET (liftIO $ try a)

-- Test

printAndFail :: (MonadIO m, Show a) => a -> m ()
printAndFail x = liftIO $ (print x) *> error (show x)

-- | 1
--   2
--   3
--   4
--   5
--   "hello"
-- 
main :: IO ()
main = do
  _ <- runEverything do
    printAndFail 1
    printAndFail 2
    pure ()

  _ <- runEverythingT do
    printAndFail 3
    printAndFail 4
    pure ()

  _ <- runEverythingT do
    printAndFail 5
    printAndFail 6

  print "hello"