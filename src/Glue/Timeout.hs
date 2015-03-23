{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Glue.Timeout(
    TimeoutOptions
  , TimeoutException(..)
  , defaultTimeoutOptions
  , addTimeout
  , timeoutDescription
  , timeoutLimitMs
) where

import Data.Typeable
import Glue.Types
import Control.Concurrent.Lifted
import Control.Exception.Lifted
import Control.Monad.Trans.Control

data TimeoutOptions = TimeoutOptions {
  timeoutDescription  :: String,
  timeoutLimitMs      :: Int
}

defaultTimeoutOptions :: TimeoutOptions
defaultTimeoutOptions = TimeoutOptions { timeoutDescription = "Service call timed out.", timeoutLimitMs = 30000 }

data TimeoutException = TimeoutException String deriving (Eq, Show, Typeable)
instance Exception TimeoutException

addTimeout :: (MonadBaseControl IO m) => TimeoutOptions -> BasicService m a b -> BasicService m a b
addTimeout options service = (\request -> do
  currentThreadId <- myThreadId
  timeoutThreadId <- fork $ do
                              threadDelay (1000 * timeoutLimitMs options)
                              throwTo currentThreadId (TimeoutException $ timeoutDescription options)
  finally (service request) (killThread timeoutThreadId))