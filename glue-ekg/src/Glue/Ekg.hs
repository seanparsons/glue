{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting the recording of various stats about a service using "System.Metrics".
module Glue.Ekg(
    recordDistribution
  , recordAttempts
  , recordSuccesses
  , recordFailures
  , recordLastRequest
  , recordLastResult
) where

import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.Trans.Control
import           Data.Text
import           Data.Time.Clock.POSIX
import           Glue.Types
import           System.Metrics
import qualified System.Metrics.Counter      as MC
import qualified System.Metrics.Distribution as MD
import qualified System.Metrics.Label        as ML

currentTime :: (MonadBaseControl IO m) => m Double
currentTime = do
                time <- liftBase $ getPOSIXTime
                return $ realToFrac time

-- | Record the timings for service invocations with a 'Distribution' held in the passed in 'Store'..
recordDistribution :: (MonadBaseControl IO m, MonadBaseControl IO n)
                   => Store                   -- ^ 'Store' where the 'Distribution' will reside.
                   -> Text                    -- ^ The name to associate the 'Distribution' with.
                   -> BasicService m a b      -- ^ Base service to record stats for.
                   -> n (BasicService m a b)
recordDistribution store name service = do
  dist <- liftBase $ createDistribution name store
  let timedService req = do
                            before <- currentTime
                            let recordTime = do
                                                after <- currentTime
                                                liftBase $ MD.add dist (after - before)
                            finally (service req) recordTime
  return timedService

-- | Increments a counter with a 'Counter' held in the passed in 'Store' for each time the service is called.
recordAttempts :: (MonadBaseControl IO m, MonadBaseControl IO n)
               => Store                   -- ^ 'Store' where the 'Counter' will reside.
               -> Text                    -- ^ The name to associate the 'Counter' with.
               -> BasicService m a b      -- ^ Base service to record stats for.
               -> n (BasicService m a b)
recordAttempts store name service = do
  counter <- liftBase $ createCounter name store
  let countedService req = do
                              liftBase $ MC.inc counter
                              service req
  return countedService

-- | Increments a counter with a 'Counter' held in the passed in 'Store' for each time the service successfully returns.
recordSuccesses :: (MonadBaseControl IO m, MonadBaseControl IO n)
                => Store                   -- ^ 'Store' where the 'Counter' will reside.
                -> Text                    -- ^ The name to associate the 'Counter' with.
                -> BasicService m a b      -- ^ Base service to record stats for.
                -> n (BasicService m a b)
recordSuccesses store name service = do
  counter <- liftBase $ createCounter name store
  let countedService req = do
                              result <- service req
                              liftBase $ MC.inc counter
                              return result
  return countedService

-- | Increments a counter with a 'Counter' held in the passed in 'Store' for each time the service fails.
recordFailures :: (MonadBaseControl IO m, MonadBaseControl IO n)
               => Store                   -- ^ 'Store' where the 'Counter' will reside.
               -> Text                    -- ^ The name to associate the 'Counter' with.
               -> BasicService m a b      -- ^ Base service to record stats for.
               -> n (BasicService m a b)
recordFailures store name service = do
  counter <- liftBase $ createCounter name store
  let countedService req = onException (service req) (liftBase $ MC.inc counter)
  return countedService

-- | Sets a 'Label' held in the passed in 'Store' for each request the service receives.
recordLastRequest :: (MonadBaseControl IO m, MonadBaseControl IO n, Show a)
                  => Store                   -- ^ 'Store' where the 'Label' will reside.
                  -> Text                    -- ^ The name to associate the 'Label' with.
                  -> BasicService m a b      -- ^ Base service to record stats for.
                  -> n (BasicService m a b)
recordLastRequest store name service = do
  label <- liftBase $ createLabel name store
  let requestRecordingService req = do
                                      liftBase $ ML.set label $ pack $ show req
                                      service req
  return requestRecordingService

-- | Sets a 'Label' held in the passed in 'Store' for each successful result the service returns.
recordLastResult :: (MonadBaseControl IO m, MonadBaseControl IO n, Show b)
                 => Store                   -- ^ 'Store' where the 'Label' will reside.
                 -> Text                    -- ^ The name to associate the 'Label' with.
                 -> BasicService m a b      -- ^ Base service to record stats for.
                 -> n (BasicService m a b)
recordLastResult store name service = do
  label <- liftBase $ createLabel name store
  let resultRecordingService req = do
                                      result <- service req
                                      liftBase $ ML.set label $ pack $ show result
                                      return result
  return resultRecordingService
