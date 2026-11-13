{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting adding timeouts to a given service.
module Glue.Timeout
  ( TimeoutOptions (..)
  , TimeoutException (..)
  , defaultTimeoutOptions
  , addTimeout
  , addTimeoutEither
  ) where

import           Glue.Clock
import           Glue.Failure
import           Glue.Types
import           UnliftIO           (Exception, MonadUnliftIO)
import           UnliftIO.Exception (throwIO)

-- | Options for determining behaviour of services with a timeout.
data TimeoutOptions m = TimeoutOptions
  { timeoutDescription :: String
  -- ^ Description added to the t'TimeoutException' thrown when the timeout is exceeded.
  , timeoutLimitMs     :: Int
  -- ^ Timeout in milliseconds.
  , timeoutClock       :: Clock m
  -- ^ Clock used to measure the timeout.
  }

-- | Default instance of t'TimeoutOptions' with a timeout of 30 seconds.
defaultTimeoutOptions :: MonadUnliftIO m => TimeoutOptions m
defaultTimeoutOptions =
  TimeoutOptions
    { timeoutDescription = "Service call timed out."
    , timeoutLimitMs     = 30000
    , timeoutClock       = systemClock
    }

-- | Exception thrown when the timeout is exceeded.
newtype TimeoutException = TimeoutException String
  deriving (Eq, Show)

instance Exception TimeoutException

timeoutVia :: Monad m => m r -> TimeoutOptions m -> BasicService m a r -> BasicService m a r
timeoutVia onTimeout options service request = do
  maybeResult <- clockTimeout (timeoutClock options) (1000 * timeoutLimitMs options) (service request)
  maybe onTimeout pure maybeResult

-- | Function for producing services protected with a timeout.
addTimeout
  :: MonadUnliftIO m
  => TimeoutOptions m
  -- ^ Options to configure the timeout.
  -> BasicService m a b
  -- ^ Service to protect with a timeout.
  -> BasicService m a b
addTimeout options = timeoutVia (throwIO $ TimeoutException $ timeoutDescription options) options

-- | Variant of 'addTimeout' for services returning failures as values, returning the given
-- failure when the timeout is exceeded instead of throwing.
addTimeoutEither
  :: MonadUnliftIO m
  => TimeoutOptions m
  -- ^ Options to configure the timeout ('timeoutDescription' is unused).
  -> e
  -- ^ Failure returned when the timeout is exceeded.
  -> EitherService m a e b
  -- ^ Service to protect with a timeout.
  -> EitherService m a e b
addTimeoutEither options onTimeout = timeoutVia (pure (Left onTimeout)) options
