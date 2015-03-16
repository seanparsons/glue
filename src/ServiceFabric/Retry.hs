{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ServiceFabric.Retry where

import Control.Exception.Base(SomeException)
import Data.Typeable
import ServiceFabric.Types
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.CatchIO

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

data RetryException = RetryException String deriving (Eq, Show, Typeable)
instance Exception RetryException

retryingService :: (MonadCatchIO m) => RetryOptions a -> BasicService m a b -> BasicService m a b
retryingService options service =
  let attempt attemptCount request  = if (retryAllowed options) request && maxAttempts <= attemptCount
                                      then catch (service request) (\(_ :: SomeException) -> (wait attemptCount) >> (attempt (attemptCount + 1) request))
                                      else service request
      maxAttempts                   = maximumRetries options
      wait attemptCount             = liftIO $ threadDelay $ round $ fromIntegral (retryInitialWaitTimeMs options) * ((retryWaitTimeMultiplier options) ^ attemptCount)
  in  attempt 1