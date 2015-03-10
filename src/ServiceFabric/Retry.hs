{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ServiceFabric.Retry where

import Data.Typeable
import ServiceFabric.Types
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.CatchIO

data RetryOptions a = RetryOptions {
  retryDescription        :: String,
  retryAllowed            :: a -> Bool,
  retryInitialWaitTimeMs  :: Int,
  maximumRetries          :: Int,
  retryWaitTimeMultiplier :: Double
}

defaultRetryOptions :: RetryOptions a
defaultRetryOptions = RetryOptions { 
    retryDescription        = "Exceeded maximum number of retries." 
  , retryAllowed            = (\_ -> True)
  , retryInitialWaitTimeMs  = 0
  , maximumRetries          = 3
  , retryWaitTimeMultiplier = 0
  }

data RetryException = RetryException String deriving (Eq, Show, Typeable)
instance Exception RetryException

