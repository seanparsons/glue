{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

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

data RetryOptions a = RetryOptions {
  retryAllowed            :: a -> Bool,
  retryInitialWaitTimeMs  :: Int,
  maximumRetries          :: Int, 
  retryWaitTimeMultiplier :: Double
}

defaultRetryOptions :: RetryOptions a
defaultRetryOptions = RetryOptions { 
    retryAllowed            = (\_ -> True)
  , retryInitialWaitTimeMs  = 0
  , maximumRetries          = 3
  , retryWaitTimeMultiplier = 0
  }

-- |Retries a call to a service multiple times, potentially backing off wait times between subsequent calls.
retryingService :: (MonadBaseControl IO m) => RetryOptions a -> BasicService m a b -> BasicService m a b
retryingService options service =
  let attempt retryCount request  = if (retryAllowed options) request && maxRetries > retryCount
                                      then catch (service request) (\(_ :: SomeException) -> (wait (retryCount + 1)) >> (attempt (retryCount + 1) request))
                                      else service request
      maxRetries                  = maximumRetries options
      wait retryCount             = threadDelay $ round $ fromIntegral (retryInitialWaitTimeMs options) * ((retryWaitTimeMultiplier options) ^ retryCount)
  in  attempt 0