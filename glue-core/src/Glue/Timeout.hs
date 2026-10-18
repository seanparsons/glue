{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting adding timeouts to a given service.
module Glue.Timeout
  ( TimeoutOptions (..)
  , TimeoutException (..)
  , defaultTimeoutOptions
  , addTimeout
  ) where

import           Glue.Types
import           UnliftIO           (Exception, MonadUnliftIO)
import           UnliftIO.Exception (throwIO)
import           UnliftIO.Timeout   (timeout)

-- | Options for determining behaviour of services with a timeout.
data TimeoutOptions = TimeoutOptions
  { timeoutDescription :: String
  -- ^ Description added to the t'TimeoutException' thrown when the timeout is exceeded.
  , timeoutLimitMs     :: Int
  -- ^ Timeout in milliseconds.
  }
  deriving (Eq, Show)

-- | Default instance of t'TimeoutOptions' with a timeout of 30 seconds.
defaultTimeoutOptions :: TimeoutOptions
defaultTimeoutOptions =
  TimeoutOptions
    { timeoutDescription = "Service call timed out."
    , timeoutLimitMs     = 30000
    }

-- | Exception thrown when the timeout is exceeded.
newtype TimeoutException = TimeoutException String
  deriving (Eq, Show)

instance Exception TimeoutException

-- | Function for producing services protected with a timeout.
addTimeout
  :: MonadUnliftIO m
  => TimeoutOptions
  -- ^ Options to configure the timeout.
  -> BasicService m a b
  -- ^ Service to protect with a timeout.
  -> BasicService m a b
addTimeout options service request = do
  maybeResult <- timeout (1000 * timeoutLimitMs options) (service request)
  maybe (throwIO $ TimeoutException $ timeoutDescription options) pure maybeResult
