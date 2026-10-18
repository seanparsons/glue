{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing retry functionality, allowing the construction of services that attempt multiple times in case of transient failure.
module Glue.Retry
  ( RetryOptions (..)
  , defaultRetryOptions
  , retryingService
  ) where

import           Control.Monad       (when)
import           Glue.Types
import           UnliftIO            (MonadUnliftIO, SomeAsyncException,
                                      SomeException, fromException)
import           UnliftIO.Concurrent (threadDelay)
import           UnliftIO.Exception  (catch, throwIO)

-- | Options for determining behaviour of retrying services.
data RetryOptions a = RetryOptions
  { retryAllowed            :: a -> Bool
  -- ^ Predicate for determining if we can retry a call, can be used to prevent retries on non-idempotent operations.
  , retryInitialWaitTimeMs  :: Int
  -- ^ Amount of time (in milliseconds) to wait after the first failure.
  , maximumRetries          :: Int
  -- ^ The upper bound on how many retry attempts to make when invoking the service.
  , retryWaitTimeMultiplier :: Double
  -- ^ How much to multiply 'retryInitialWaitTimeMs' by for each subsequent retry.
  }

-- | Defaulted options for retrying 3 times with no wait time.
defaultRetryOptions :: RetryOptions a
defaultRetryOptions =
  RetryOptions
    { retryAllowed            = const True
    , retryInitialWaitTimeMs  = 0
    , maximumRetries          = 3
    , retryWaitTimeMultiplier = 0
    }

possibleAsyncException :: SomeException -> Maybe SomeAsyncException
possibleAsyncException = fromException

-- | Retries a call to a service multiple times, potentially backing off wait times between subsequent calls.
-- Asynchronous exceptions don't result in a retry; they are immediately rethrown.
retryingService
  :: forall m a b.
     MonadUnliftIO m
  => RetryOptions a
  -- ^ Instance of t'RetryOptions' to configure the retry functionality.
  -> BasicService m a b
  -- ^ The service to perform retries of.
  -> BasicService m a b
retryingService options service =
  let catchHandler rc r e =
        case possibleAsyncException e of
          Just ae -> throwIO ae
          Nothing -> wait rc >> attempt (rc + 1) r
      attempt retryCount request =
        if retryAllowed options request && maxRetries > retryCount
          then catch (service request) (catchHandler retryCount request)
          else service request
      maxRetries = maximumRetries options
      initialUs  = retryInitialWaitTimeMs options * 1000
      multiplier =
        if retryWaitTimeMultiplier options <= 0
          then 1.0
          else retryWaitTimeMultiplier options
      wait :: Int -> m ()
      wait retryCount =
        when (initialUs > 0) $
          threadDelay $ round $ fromIntegral initialUs * (multiplier ^ retryCount)
  in  attempt 0
