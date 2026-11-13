{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing retry functionality, allowing the construction of services that attempt multiple times in case of transient failure.
--
-- Whether to retry is decided by the failure ('retryOn') as well as the request ('retryAllowed').
-- Waits between attempts follow a 'Backoff', optionally capped and jittered, and a retry whose wait
-- would pass the caller's deadline ('retryRemainingMs') is skipped, returning the last failure.
-- Asynchronous exceptions are never retried.
module Glue.Retry
  ( RetryOptions (..)
  , Backoff (..)
  , Jitter (..)
  , defaultRetryOptions
  , backoffDelayMs
  , retryingService
  , retryingServiceEither
  ) where

import           Glue.Clock
import           Glue.Failure
import           Glue.Types
import           System.Random      (randomRIO)
import           UnliftIO           (MonadUnliftIO, SomeException, liftIO)

-- | How the wait before each retry grows.
data Backoff
  = ConstantBackoff Int
  -- ^ The same wait (in milliseconds) before every retry.
  | LinearBackoff Int Int
  -- ^ An initial wait, growing by the second value (both in milliseconds) for each retry.
  | ExponentialBackoff Int Double
  -- ^ An initial wait in milliseconds, multiplied by the second value for each retry.
  deriving (Eq, Show)

-- | Randomisation of the wait, so that many clients don't retry in lockstep.
-- See <https://aws.amazon.com/blogs/architecture/exponential-backoff-and-jitter/>.
data Jitter
  = NoJitter
  -- ^ Wait exactly the backoff.
  | FullJitter
  -- ^ Wait a uniformly random time between zero and the backoff.
  | EqualJitter
  -- ^ Wait half the backoff plus a uniformly random time up to the other half.
  deriving (Eq, Show)

-- | Options for determining behaviour of retrying services.
data RetryOptions m a failure = RetryOptions
  { retryAllowed     :: a -> Bool
  -- ^ Predicate for determining if we can retry a call, can be used to prevent retries on non-idempotent operations.
  , retryOn          :: failure -> Bool
  -- ^ Predicate on the failure: retry an unreachable backend or a timeout, say, but not a rejected request.
  , maximumRetries   :: Int
  -- ^ The upper bound on how many retry attempts to make when invoking the service.
  , retryBackoff     :: Backoff
  -- ^ How long to wait before each retry.
  , retryMaxDelayMs  :: Maybe Int
  -- ^ Ceiling on the wait before any retry, applied before jitter.
  , retryJitter      :: Jitter
  -- ^ Randomisation applied to the capped wait.
  , retryRandom      :: Int -> m Int
  -- ^ Source of randomness for jitter: a uniformly random value between zero and the argument inclusive.
  , retryRemainingMs :: m (Maybe Int)
  -- ^ Time left (in milliseconds) before the caller's deadline, if there is one. A retry whose wait
  -- would pass it is skipped. 'remainingUntil' builds this from a deadline on a 'Clock'.
  , onRetry          :: Int -> failure -> Int -> m ()
  -- ^ Called before each retry with the retry number (from 1), the failure and the chosen wait in milliseconds.
  , retryClock       :: Clock m
  -- ^ Clock used to wait between attempts.
  }

-- | Defaulted options for retrying any failure 3 times with no wait time and no deadline.
defaultRetryOptions :: MonadUnliftIO m => RetryOptions m a failure
defaultRetryOptions =
  RetryOptions
    { retryAllowed     = const True
    , retryOn          = const True
    , maximumRetries   = 3
    , retryBackoff     = ConstantBackoff 0
    , retryMaxDelayMs  = Nothing
    , retryJitter      = NoJitter
    , retryRandom      = \n -> liftIO $ randomRIO (0, max 0 n)
    , retryRemainingMs = pure Nothing
    , onRetry          = \_ _ _ -> pure ()
    , retryClock       = systemClock
    }

-- | The un-jittered, uncapped wait in milliseconds before the retry with the given index (from 0).
backoffDelayMs :: Backoff -> Int -> Int
backoffDelayMs (ConstantBackoff delay) _ = max 0 delay
backoffDelayMs (LinearBackoff initial step) n = max 0 (initial + step * n)
backoffDelayMs (ExponentialBackoff initial multiplier) n =
  let factor  = if multiplier <= 0 then 1 else multiplier
      delay   = fromIntegral (max 0 initial) * factor ^ n :: Double
  in  if delay >= fromIntegral (maxBound :: Int) then maxBound else round delay

chooseDelay :: Monad m => RetryOptions m a failure -> Int -> m Int
chooseDelay options n = do
  let capped = maybe id (min . max 0) (retryMaxDelayMs options) (backoffDelayMs (retryBackoff options) n)
  case retryJitter options of
    NoJitter    -> pure capped
    FullJitter  -> clamp capped <$> retryRandom options capped
    EqualJitter -> do
      let half = capped `div` 2
      (+ half) . clamp (capped - half) <$> retryRandom options (capped - half)
  where
    clamp upper = max 0 . min upper

retryVia :: Monad m => FailureMode m r failure -> RetryOptions m a failure -> BasicService m a r -> BasicService m a r
retryVia mode options service request = attempt 0
  where
    attempt retryCount = do
      outcome <- observeCall mode (service request)
      case outcome of
        Right result -> pure result
        Left failure
          | retryCount < maximumRetries options && retryAllowed options request && retryOn options failure -> do
              delay     <- chooseDelay options retryCount
              remaining <- retryRemainingMs options
              if maybe False (\left -> left <= 0 || delay > left) remaining
                then reraise mode failure
                else do
                  onRetry options (retryCount + 1) failure delay
                  clockSleep (retryClock options) (delay * 1000)
                  attempt (retryCount + 1)
          | otherwise -> reraise mode failure

-- | Retries a call to a service multiple times, potentially backing off wait times between subsequent calls.
-- Asynchronous exceptions don't result in a retry; they are immediately rethrown.
retryingService
  :: MonadUnliftIO m
  => RetryOptions m a SomeException
  -- ^ Instance of t'RetryOptions' to configure the retry functionality.
  -> BasicService m a b
  -- ^ The service to perform retries of.
  -> BasicService m a b
retryingService = retryVia exceptionFailures

-- | Variant of 'retryingService' for services returning failures as values: @Left@ results are
-- judged by 'retryOn', and the last @Left@ is returned once retrying stops. Exceptions are not caught.
retryingServiceEither
  :: Monad m
  => RetryOptions m a e
  -- ^ Instance of t'RetryOptions' to configure the retry functionality.
  -> EitherService m a e b
  -- ^ The service to perform retries of.
  -> EitherService m a e b
retryingServiceEither = retryVia valueFailures
