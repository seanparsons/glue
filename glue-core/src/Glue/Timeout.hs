{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting adding timeouts to a given service.
module Glue.Timeout(
    TimeoutOptions
  , TimeoutException(..)
  , defaultTimeoutOptions
  , addTimeout
  , timeoutDescription
  , timeoutLimitMs
) where

import           Control.Concurrent.Lifted
import           Control.Exception.Lifted
import           Control.Monad.Trans.Control
import           Data.Typeable
import           Glue.Types

-- | Options for determining behaviour of services with a timeout.
data TimeoutOptions = TimeoutOptions {
    timeoutDescription :: String       -- ^ Description added to the 'TimeoutException' thrown when the timeout is exceeded.
  , timeoutLimitMs     :: Int          -- ^ Timeout in milliseconds.
}

-- | Default instance of 'TimeoutOptions' with a timeout of 30 seconds.
defaultTimeoutOptions :: TimeoutOptions
defaultTimeoutOptions = TimeoutOptions { timeoutDescription = "Service call timed out.", timeoutLimitMs = 30000 }

-- | Exception thrown when the timeout is exceeded.
data TimeoutException = TimeoutException String deriving (Eq, Show, Typeable)
instance Exception TimeoutException

-- | Function for producing services protected with a timeout.
addTimeout :: (MonadBaseControl IO m)
           => TimeoutOptions        -- ^ Options to configure the timeout.
           -> BasicService m a b    -- ^ Service to protect with a timeout.
           -> BasicService m a b
addTimeout options service = (\request -> do
  currentThreadId <- myThreadId
  timeoutThreadId <- fork $ do
                              threadDelay (1000 * timeoutLimitMs options)
                              throwTo currentThreadId (TimeoutException $ timeoutDescription options)
  finally (service request) (killThread timeoutThreadId))
