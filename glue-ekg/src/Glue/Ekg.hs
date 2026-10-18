{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting the recording of various stats about a service using "System.Metrics".
module Glue.Ekg
  ( recordDistribution
  , recordAttempts
  , recordSuccesses
  , recordFailures
  , recordLastRequest
  , recordLastResult
  ) where

import           Data.Text                   (Text, pack)
import           Data.Time.Clock.POSIX       (getPOSIXTime)
import           Glue.Types
import           System.Metrics              (Store, createCounter,
                                              createDistribution, createLabel)
import qualified System.Metrics.Counter      as MC
import qualified System.Metrics.Distribution as MD
import qualified System.Metrics.Label        as ML
import           UnliftIO                    (MonadIO, MonadUnliftIO, liftIO)
import           UnliftIO.Exception          (finally, onException)

currentTime :: MonadIO m => m Double
currentTime = do
  time <- liftIO getPOSIXTime
  pure $ realToFrac time

-- | Record the timings for service invocations with a 'MD.Distribution' held in the passed in 'Store'.
recordDistribution
  :: (MonadUnliftIO m, MonadIO n)
  => Store
  -- ^ 'Store' where the 'MD.Distribution' will reside.
  -> Text
  -- ^ The name to associate the 'MD.Distribution' with.
  -> BasicService m a b
  -- ^ Base service to record stats for.
  -> n (BasicService m a b)
recordDistribution store name service = do
  dist <- liftIO $ createDistribution name store
  let timedService req = do
        before <- currentTime
        let recordTime = do
              after <- currentTime
              liftIO $ MD.add dist (after - before)
        finally (service req) recordTime
  pure timedService

-- | Increments a counter with a 'MC.Counter' held in the passed in 'Store' for each time the service is called.
recordAttempts
  :: (MonadIO m, MonadIO n)
  => Store
  -- ^ 'Store' where the 'MC.Counter' will reside.
  -> Text
  -- ^ The name to associate the 'MC.Counter' with.
  -> BasicService m a b
  -- ^ Base service to record stats for.
  -> n (BasicService m a b)
recordAttempts store name service = do
  counter <- liftIO $ createCounter name store
  let countedService req = do
        liftIO $ MC.inc counter
        service req
  pure countedService

-- | Increments a counter with a 'MC.Counter' held in the passed in 'Store' for each time the service successfully returns.
recordSuccesses
  :: (MonadIO m, MonadIO n)
  => Store
  -- ^ 'Store' where the 'MC.Counter' will reside.
  -> Text
  -- ^ The name to associate the 'MC.Counter' with.
  -> BasicService m a b
  -- ^ Base service to record stats for.
  -> n (BasicService m a b)
recordSuccesses store name service = do
  counter <- liftIO $ createCounter name store
  let countedService req = do
        result <- service req
        liftIO $ MC.inc counter
        pure result
  pure countedService

-- | Increments a counter with a 'MC.Counter' held in the passed in 'Store' for each time the service fails.
recordFailures
  :: (MonadUnliftIO m, MonadIO n)
  => Store
  -- ^ 'Store' where the 'MC.Counter' will reside.
  -> Text
  -- ^ The name to associate the 'MC.Counter' with.
  -> BasicService m a b
  -- ^ Base service to record stats for.
  -> n (BasicService m a b)
recordFailures store name service = do
  counter <- liftIO $ createCounter name store
  let countedService req = onException (service req) (liftIO $ MC.inc counter)
  pure countedService

-- | Sets a 'ML.Label' held in the passed in 'Store' for each request the service receives.
recordLastRequest
  :: (MonadIO m, MonadIO n, Show a)
  => Store
  -- ^ 'Store' where the 'ML.Label' will reside.
  -> Text
  -- ^ The name to associate the 'ML.Label' with.
  -> BasicService m a b
  -- ^ Base service to record stats for.
  -> n (BasicService m a b)
recordLastRequest store name service = do
  label <- liftIO $ createLabel name store
  let requestRecordingService req = do
        liftIO $ ML.set label $ pack $ show req
        service req
  pure requestRecordingService

-- | Sets a 'ML.Label' held in the passed in 'Store' for each successful result the service returns.
recordLastResult
  :: (MonadIO m, MonadIO n, Show b)
  => Store
  -- ^ 'Store' where the 'ML.Label' will reside.
  -> Text
  -- ^ The name to associate the 'ML.Label' with.
  -> BasicService m a b
  -- ^ Base service to record stats for.
  -> n (BasicService m a b)
recordLastResult store name service = do
  label <- liftIO $ createLabel name store
  let resultRecordingService req = do
        result <- service req
        liftIO $ ML.set label $ pack $ show result
        pure result
  pure resultRecordingService
