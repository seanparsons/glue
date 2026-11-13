{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A virtual 'Clock' for deterministic tests: time only moves when 'advanceClock' is called,
-- and sleepers are woken in deadline order as it passes them.
--
-- A typical spec starts the code under test, waits for it to block with 'waitForSleepers',
-- then steps time with 'advanceClock' and asserts on what happened.
module Glue.Clock.Virtual
  ( VirtualClock
  , newVirtualClock
  , virtualClock
  , advanceClock
  , virtualNow
  , sleeperCount
  , waitForSleepers
  ) where

import           Control.Monad  (when)
import           Data.Int       (Int64)
import qualified Data.Set       as S
import           Glue.Clock
import           UnliftIO       (MonadIO, MonadUnliftIO)
import           UnliftIO.Async (race)
import           UnliftIO.Exception (onException)
import           UnliftIO.STM   (TVar, atomically, modifyTVar', newTVarIO,
                                 readTVar, retrySTM, stateTVar, writeTVar)

-- | Virtual time, starting at zero, plus the sleepers waiting on it.
data VirtualClock = VirtualClock
  { vcNow      :: TVar Int64
  , vcSleepers :: TVar (S.Set (Int64, Int))
  -- ^ Each sleeper as (deadline, unique id).
  , vcNextId   :: TVar Int
  }

-- | Creates a virtual clock at time zero with no sleepers.
newVirtualClock :: MonadIO m => m VirtualClock
newVirtualClock = VirtualClock <$> newTVarIO 0 <*> newTVarIO S.empty <*> newTVarIO 0

-- | A 'Clock' reading from the virtual clock.
virtualClock :: MonadUnliftIO m => VirtualClock -> Clock m
virtualClock vc =
  Clock
    { clockNow     = virtualNow vc
    , clockSleep   = virtualSleep vc
    , clockTimeout = virtualTimeout vc
    }

-- | The current virtual time in microseconds.
virtualNow :: MonadIO m => VirtualClock -> m Int64
virtualNow vc = atomically $ readTVar (vcNow vc)

-- | The number of threads currently sleeping on the clock.
sleeperCount :: MonadIO m => VirtualClock -> m Int
sleeperCount vc = atomically $ S.size <$> readTVar (vcSleepers vc)

-- | Blocks until at least the given number of threads are sleeping on the clock.
waitForSleepers :: MonadIO m => VirtualClock -> Int -> m ()
waitForSleepers vc n = atomically $ do
  sleepers <- readTVar (vcSleepers vc)
  when (S.size sleepers < n) retrySTM

virtualSleep :: MonadUnliftIO m => VirtualClock -> Int -> m ()
virtualSleep vc us
  | us <= 0   = pure ()
  | otherwise = do
      sleeper <- atomically $ do
        now <- readTVar (vcNow vc)
        uid <- stateTVar (vcNextId vc) (\i -> (i, i + 1))
        let entry = (now + fromIntegral us, uid)
        modifyTVar' (vcSleepers vc) (S.insert entry)
        pure entry
      let remove = atomically $ modifyTVar' (vcSleepers vc) (S.delete sleeper)
      -- Waking and deregistering happen in one transaction, which 'advanceClock' relies on.
      atomically (do
          now <- readTVar (vcNow vc)
          when (now < fst sleeper) retrySTM
          modifyTVar' (vcSleepers vc) (S.delete sleeper))
        `onException` remove

virtualTimeout :: MonadUnliftIO m => VirtualClock -> Int -> m b -> m (Maybe b)
virtualTimeout vc us action
  | us < 0    = Just <$> action
  | us == 0   = pure Nothing
  | otherwise = either (const Nothing) Just <$> race (virtualSleep vc us) action

-- | Moves time forward by the given number of microseconds. Time steps to each sleeper's
-- deadline in order, waiting at each step until every sleeper due by then has woken.
advanceClock :: MonadIO m => VirtualClock -> Int -> m ()
advanceClock vc us = do
  target <- atomically $ (+ fromIntegral us) <$> readTVar (vcNow vc)
  let step = do
        stepped <- atomically $ do
          sleepers <- readTVar (vcSleepers vc)
          case S.lookupMin sleepers of
            Just (deadline, _) | deadline <= target -> do
              now <- readTVar (vcNow vc)
              writeTVar (vcNow vc) (max now deadline)
              pure True
            _ -> do
              writeTVar (vcNow vc) target
              pure False
        when stepped $ do
          atomically $ do
            now      <- readTVar (vcNow vc)
            sleepers <- readTVar (vcSleepers vc)
            case S.lookupMin sleepers of
              Just (deadline, _) | deadline <= now -> retrySTM
              _                                    -> pure ()
          step
  step
