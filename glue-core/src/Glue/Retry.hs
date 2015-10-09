{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Module containing retry functionality, allowing the construction of services that attempt multiple times in case of transient failure.
module Glue.Retry(
    RetryOptions
  , defaultRetryOptions
  , retryingService
  , retryAllowed
  , retryInitialWaitTimeMs
  , maximumRetries
  , retryWaitTimeMultiplier
) where

import Glue.Types
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad.Trans.Control

-- | Options for determining behaviour of retrying services.
data RetryOptions a = RetryOptions {
  retryAllowed            :: a -> Bool,   -- ^ Predicate for determining if we can retry a call, can be used to prevent retries on non-idempotent operations.
  retryInitialWaitTimeMs  :: Int,         -- ^ Amount of time to wait after the first failure.
  maximumRetries          :: Int,         -- ^ The upper bound on how many attempts to make when invoking the service.
  retryWaitTimeMultiplier :: Double       -- ^ How much to multiply 'retryInitialWaitTimeMs' by for each number of times the service has retried.
}

-- | Defaulted options for retrying 3 times with no wait time.
defaultRetryOptions :: RetryOptions a
defaultRetryOptions = RetryOptions {
    retryAllowed            = (\_ -> True)
  , retryInitialWaitTimeMs  = 0
  , maximumRetries          = 3
  , retryWaitTimeMultiplier = 0
  }

possibleAsyncException :: SomeException -> Maybe SomeAsyncException
possibleAsyncException = fromException

-- | Retries a call to a service multiple times, potentially backing off wait times between subsequent calls.
-- | Asynchronous exceptions don't result in a retry, they are immediately rethrown.
retryingService :: (MonadBaseControl IO m)
                => RetryOptions a             -- ^ Instance of 'RetryOptions' to configure the retry functionality.
                -> BasicService m a b         -- ^ The service to perform retries of.
                -> BasicService m a b
retryingService options service =
  let catchHandler rc r e         = case possibleAsyncException e of
                                                                    Just ae -> throw ae
                                                                    Nothing -> wait (rc + 1) >> attempt (rc + 1) r
      attempt retryCount request  = if (retryAllowed options) request && maxRetries > retryCount
                                    then catch (service request) (catchHandler retryCount request)
                                    else service request
      maxRetries                  = maximumRetries options
      wait retryCount             = threadDelay $ round $ fromIntegral (retryInitialWaitTimeMs options) * ((retryWaitTimeMultiplier options) ^ retryCount)
  in  attempt 0