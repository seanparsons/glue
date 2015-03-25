{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.Stats(
    recordDistribution
  , recordAttempts
  , recordSuccesses
  , recordFailures
  , recordLastRequest
  , recordLastResult
) where

import Control.Exception.Lifted
import Control.Monad.Base
import Data.Text
import Data.Time.Clock.POSIX
import Glue.Types
import Control.Monad.Trans.Control
import System.Metrics
import qualified System.Metrics.Distribution as MD
import qualified System.Metrics.Counter as MC
import qualified System.Metrics.Label as ML

currentTime :: (MonadBaseControl IO m) => m Double
currentTime = do
                time <- liftBase $ getPOSIXTime
                return $ realToFrac time

recordDistribution :: (MonadBaseControl IO m, MonadBaseControl IO n) => Store -> Text -> BasicService m a b -> n (BasicService m a b)
recordDistribution store name service = do
  dist <- liftBase $ createDistribution name store
  let timedService req = do
                            before <- currentTime
                            let recordTime = do
                                                after <- currentTime
                                                liftBase $ MD.add dist (after - before)
                            finally (service req) recordTime 
  return timedService

recordAttempts :: (MonadBaseControl IO m, MonadBaseControl IO n) => Store -> Text -> BasicService m a b -> n (BasicService m a b)
recordAttempts store name service = do
  counter <- liftBase $ createCounter name store
  let countedService req = do
                              liftBase $ MC.inc counter
                              service req
  return countedService


recordSuccesses :: (MonadBaseControl IO m, MonadBaseControl IO n) => Store -> Text -> BasicService m a b -> n (BasicService m a b)
recordSuccesses store name service = do
  counter <- liftBase $ createCounter name store
  let countedService req = do
                              result <- service req
                              liftBase $ MC.inc counter
                              return result
  return countedService

recordFailures :: (MonadBaseControl IO m, MonadBaseControl IO n) => Store -> Text -> BasicService m a b -> n (BasicService m a b)
recordFailures store name service = do
  counter <- liftBase $ createCounter name store
  let countedService req = onException (service req) (liftBase $ MC.inc counter)
  return countedService

recordLastRequest :: (MonadBaseControl IO m, MonadBaseControl IO n, Show a) => Store -> Text -> BasicService m a b -> n (BasicService m a b)
recordLastRequest store name service = do
  label <- liftBase $ createLabel name store
  let requestRecordingService req = do
                                      liftBase $ ML.set label $ pack $ show req
                                      service req
  return requestRecordingService

recordLastResult :: (MonadBaseControl IO m, MonadBaseControl IO n, Show b) => Store -> Text -> BasicService m a b -> n (BasicService m a b)
recordLastResult store name service = do
  label <- liftBase $ createLabel name store
  let resultRecordingService req = do
                                      result <- service req
                                      liftBase $ ML.set label $ pack $ show result
                                      return result                                      
  return resultRecordingService