{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting background jobs run periodically (sweeping expired rows, flushing a queue,
-- polling for state) with a graceful stop.
--
-- Passes never overlap: the period is measured from the end of the previous pass. A pass that fails
-- with a synchronous exception is reported and the loop carries on; an asynchronous exception ends
-- the loop.
module Glue.Periodic
  ( PeriodicOptions (..)
  , PeriodicHandle (..)
  , defaultPeriodicOptions
  , periodically
  ) where

import           Control.Monad      (unless, void, when)
import           Data.Maybe         (isJust)
import           Glue.Clock
import           System.Random      (randomRIO)
import           UnliftIO           (MonadUnliftIO, SomeException, liftIO)
import           UnliftIO.Async     (async, cancel)
import           UnliftIO.Exception (finally, tryAny)
import           UnliftIO.STM       (atomically, modifyTVar', newTVarIO,
                                     readTVar, retrySTM, writeTVar)

-- | Options for a periodic job.
data PeriodicOptions m = PeriodicOptions
  { periodMs              :: Int
  -- ^ Time (in milliseconds) from the end of one pass to the start of the next.
  , runImmediately        :: Bool
  -- ^ Whether the first pass runs at once rather than after the first period.
  , periodJitterMs        :: Int
  -- ^ Up to this many milliseconds, chosen at random, are added to each period.
  , periodicRandom        :: Int -> m Int
  -- ^ Source of randomness for jitter: a uniformly random value between zero and the argument inclusive.
  , onPassFailed          :: SomeException -> m ()
  -- ^ Told of each pass that failed with a synchronous exception.
  , periodicGracePeriodMs :: Int
  -- ^ How long (in milliseconds) 'stopPeriodic' lets a running pass finish before cancelling it.
  , periodicClock         :: Clock m
  -- ^ Clock used for the period and grace period.
  }

-- | Defaults: every 60 seconds, first pass after one period, no jitter, failures ignored, and a
-- 5 second grace period.
defaultPeriodicOptions :: MonadUnliftIO m => PeriodicOptions m
defaultPeriodicOptions =
  PeriodicOptions
    { periodMs              = 60000
    , runImmediately        = False
    , periodJitterMs        = 0
    , periodicRandom        = \n -> liftIO $ randomRIO (0, max 0 n)
    , onPassFailed          = const (pure ())
    , periodicGracePeriodMs = 5000
    , periodicClock         = systemClock
    }

-- | Handle used to control a periodic job.
data PeriodicHandle m = PeriodicHandle
  { stopPeriodic    :: m Bool
  -- ^ Stops the job: no new pass starts, and a running pass gets 'periodicGracePeriodMs' to finish
  -- before it is cancelled. Returns 'True' if it stopped without cancelling a pass.
  , runNow          :: m Bool
  -- ^ Triggers a pass, queued behind any pass that is running, and waits for it to complete.
  -- Returns 'False' if the job stopped first.
  , passesCompleted :: m Int
  -- ^ Number of passes that have finished, whether they succeeded or failed.
  }

-- | Starts running the pass periodically in the background.
periodically :: forall m. MonadUnliftIO m => PeriodicOptions m -> m () -> m (PeriodicHandle m)
periodically options pass = do
  stoppingVar  <- newTVarIO False
  requestedVar <- newTVarIO (0 :: Int)
  runningVar   <- newTVarIO False
  completedVar <- newTVarIO (0 :: Int)
  endedVar     <- newTVarIO False
  let clock = periodicClock options

      waitForPeriod :: m ()
      waitForPeriod = do
        jitter <- if periodJitterMs options > 0 then periodicRandom options (periodJitterMs options) else pure 0
        let delayUs = (max 0 (periodMs options) + max 0 (min (periodJitterMs options) jitter)) * 1000
        void $ clockTimeout clock delayUs $ atomically $ do
          stopping  <- readTVar stoppingVar
          requested <- readTVar requestedVar
          unless (stopping || requested > 0) retrySTM

      runPass :: m ()
      runPass = do
        result <- tryAny pass `finally` atomically (writeTVar runningVar False >> modifyTVar' completedVar (+ 1))
        either (void . tryAny . onPassFailed options) pure result

      loop :: Bool -> m ()
      loop first = do
        unless (first && runImmediately options) waitForPeriod
        started <- atomically $ do
          stopping <- readTVar stoppingVar
          if stopping
            then pure False
            else do
              writeTVar runningVar True
              writeTVar requestedVar 0
              pure True
        when started $ runPass >> loop False

  worker <- async (loop True `finally` atomically (writeTVar endedVar True))
  pure
    PeriodicHandle
      { stopPeriodic = do
          atomically $ writeTVar stoppingVar True
          drained <- clockTimeout clock (max 0 (periodicGracePeriodMs options) * 1000) $ atomically $ do
            running <- readTVar runningVar
            when running retrySTM
          cancel worker
          pure (isJust drained)
      , runNow = do
          target <- atomically $ do
            stopping <- readTVar stoppingVar
            if stopping
              then pure Nothing
              else do
                running   <- readTVar runningVar
                completed <- readTVar completedVar
                modifyTVar' requestedVar (+ 1)
                pure (Just (completed + if running then 2 else 1))
          case target of
            Nothing -> pure False
            Just wanted -> atomically $ do
              completed <- readTVar completedVar
              ended     <- readTVar endedVar
              if completed >= wanted
                then pure True
                else if ended then pure False else retrySTM
      , passesCompleted = atomically $ readTVar completedVar
      }
