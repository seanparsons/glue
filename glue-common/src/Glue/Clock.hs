{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

-- | Module containing an injectable clock, so that combinators which read the time or sleep
-- can be driven by a virtual clock in tests (see "Glue.Clock.Virtual").
module Glue.Clock
  ( Clock (..)
  , systemClock
  , remainingUntil
  ) where

import           Data.Int            (Int64)
import           GHC.Clock           (getMonotonicTimeNSec)
import           UnliftIO            (MonadUnliftIO, liftIO)
import           UnliftIO.Concurrent (threadDelay)
import           UnliftIO.Timeout    (timeout)

-- | Source of time and sleeping used by the time-dependent combinators.
data Clock m = Clock
  { clockNow     :: m Int64
  -- ^ Current monotonic time in microseconds. Only differences between readings are meaningful.
  , clockSleep   :: Int -> m ()
  -- ^ Sleep for the given number of microseconds.
  , clockTimeout :: forall b. Int -> m b -> m (Maybe b)
  -- ^ Run an action with a limit in microseconds, as 'UnliftIO.Timeout.timeout' does:
  -- 'Nothing' if the limit passed first, a negative limit meaning no limit.
  }

-- | The real clock, built on 'getMonotonicTimeNSec', 'threadDelay' and 'timeout'.
systemClock :: MonadUnliftIO m => Clock m
systemClock =
  Clock
    { clockNow     = liftIO $ fromIntegral . (`div` 1000) <$> getMonotonicTimeNSec
    , clockSleep   = threadDelay
    , clockTimeout = timeout
    }

-- | Milliseconds left until a deadline given in the clock's microseconds (never negative),
-- in the shape the retry options use for deadline-aware backoff.
remainingUntil :: Monad m => Clock m -> Int64 -> m (Maybe Int)
remainingUntil clock deadline = do
  now <- clockNow clock
  pure $ Just $ fromIntegral $ max 0 ((deadline - now) `div` 1000)
