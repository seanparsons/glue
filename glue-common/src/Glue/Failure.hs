{-# LANGUAGE FlexibleContexts #-}

-- | Module describing how a service signals failure, so that combinators can be written once
-- and used both with services that throw and with services that return failures as values.
--
-- The exception shape treats any synchronous exception as the failure. The value shape treats
-- @Left e@ as the failure; in that shape exceptions are not inspected and propagate unchanged.
-- Asynchronous exceptions always propagate untouched in both shapes.
module Glue.Failure
  ( EitherService
  , FailureMode (..)
  , exceptionFailures
  , valueFailures
  , fromExceptTService
  , toExceptTService
  ) where

import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Glue.Types
import           UnliftIO                   (MonadUnliftIO, SomeException)
import           UnliftIO.Exception         (throwIO, tryAny)

-- | Type alias for a service that returns expected failures as values.
type EitherService m a e b = BasicService m a (Either e b)

-- | How a combinator observes the failure of a call returning @r@.
data FailureMode m r failure = FailureMode
  { observeCall :: m r -> m (Either failure r)
  -- ^ Runs a call, separating its failure (if any) from a successful result.
  , reraise     :: failure -> m r
  -- ^ Signals a failure back to the caller in this shape: throws it, or returns it as a value.
  }

-- | Failures are synchronous exceptions; asynchronous exceptions are never caught.
exceptionFailures :: MonadUnliftIO m => FailureMode m b SomeException
exceptionFailures =
  FailureMode
    { observeCall = tryAny
    , reraise     = throwIO
    }

-- | Failures are @Left@ values; exceptions are not caught at all.
valueFailures :: Monad m => FailureMode m (Either e b) e
valueFailures =
  FailureMode
    { observeCall = fmap (\r -> either Left (const (Right r)) r)
    , reraise     = pure . Left
    }

-- | Converts a service in t'ExceptT' to an 'EitherService'.
fromExceptTService :: (a -> ExceptT e m b) -> EitherService m a e b
fromExceptTService service = runExceptT . service

-- | Converts an 'EitherService' to a service in t'ExceptT'.
toExceptTService :: EitherService m a e b -> a -> ExceptT e m b
toExceptTService service = ExceptT . service
