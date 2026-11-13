{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting request hedging: dispatching further attempts if earlier ones haven't
-- answered within a delay, and completing with the first successful result.
--
-- 'hedgeAcross' races distinct backends, a primary then its backups, leaving the caller in control
-- of whether each backup may launch and informed of how every attempt ended. 'hedgingService'
-- duplicates the same call to one replicated backend. Both are built on "Glue.Race", which callers
-- with their own launch policy can drive directly.
module Glue.Hedging
  ( HedgeOptions (..)
  , defaultHedgeOptions
  , hedgeAcross
  , hedgeAcrossEither
  , HedgingOptions (..)
  , defaultHedgingOptions
  , hedgingService
  , AttemptOutcome (..)
  ) where

import           Data.Foldable      (traverse_)
import           Data.List.NonEmpty (NonEmpty (..))
import           Data.Maybe         (fromMaybe)
import           Glue.Clock
import           Glue.Failure
import           Glue.Race
import           Glue.Types
import           UnliftIO           (MonadUnliftIO, SomeException)
import           UnliftIO.Exception (throwIO)

-- | Options for hedging across distinct backends.
data HedgeOptions m failure r = HedgeOptions
  { hedgeDelayMs   :: Int
  -- ^ How long (in milliseconds) to wait for an attempt to end before launching the next backend.
  -- A failed attempt launches the next backend straight away.
  , admitLaunch    :: Int -> m Bool
  -- ^ Asked before launching the backend with this index (from 1; the primary always launches).
  -- Declining skips that backend without failing the attempts already running; the next backend
  -- is considered after a further delay.
  , onAttemptEnded :: Int -> AttemptOutcome failure r -> m ()
  -- ^ Told how each launched attempt ended, including losers that were cancelled, once each has
  -- actually finished.
  , onHedgeWon     :: Int -> m ()
  -- ^ Told which attempt's answer was returned.
  , hedgeClock     :: Clock m
  -- ^ Clock used for the delays.
  }

-- | Defaults launching each backend 100ms after the previous one with no further control.
defaultHedgeOptions :: MonadUnliftIO m => HedgeOptions m failure r
defaultHedgeOptions =
  HedgeOptions
    { hedgeDelayMs   = 100
    , admitLaunch    = const (pure True)
    , onAttemptEnded = \_ _ -> pure ()
    , onHedgeWon     = const (pure ())
    , hedgeClock     = systemClock
    }

-- | Thrown if a hedge ends with no outcome to report, which the race guarantees cannot happen.
noOutcome :: MonadUnliftIO m => m a
noOutcome = throwIO (userError "Glue.Hedging: every attempt ended without an outcome")

hedgeVia
  :: forall m a r failure.
     MonadUnliftIO m
  => FailureMode m r failure
  -> HedgeOptions m failure r
  -> NonEmpty (BasicService m a r)
  -> BasicService m a r
hedgeVia mode options (primary :| backups) request =
  withRace mode (hedgeClock options) $ \race -> do
    let finishLosers = abandonAll race >>= traverse_ (uncurry (onAttemptEnded options))

        launchNext :: [(Int, BasicService m a r)] -> m ([(Int, BasicService m a r)], Bool)
        launchNext []                      = pure ([], False)
        launchNext ((index, service) : rest) = do
          admitted <- admitLaunch options index
          if admitted
            then launch race (service request) >> pure (rest, True)
            else pure (rest, False)

        launchUntilRunning pending = do
          (pending', launched) <- launchNext pending
          if launched || null pending' then pure pending' else launchUntilRunning pending'

        loop :: [(Int, BasicService m a r)] -> Maybe failure -> m r
        loop pending firstFailure = do
          event <- awaitNext race (if null pending then Nothing else Just (hedgeDelayMs options))
          case event of
            AttemptEnded index outcome@(AttemptAnswered result) -> do
              onAttemptEnded options index outcome
              finishLosers
              onHedgeWon options index
              pure result
            AttemptEnded index outcome@(AttemptFailed failure) -> do
              onAttemptEnded options index outcome
              (pending', _) <- launchNext pending
              loop pending' (Just (fromMaybe failure firstFailure))
            AttemptEnded index outcome@(AttemptCrashed e) -> do
              onAttemptEnded options index outcome
              finishLosers
              throwIO e
            AttemptEnded index outcome@AttemptCancelled -> do
              onAttemptEnded options index outcome
              loop pending firstFailure
            RaceTimedOut -> do
              (pending', _) <- launchNext pending
              loop pending' firstFailure
            NothingRunning
              | null pending -> maybe noOutcome (reraise mode) firstFailure
              | otherwise    -> launchUntilRunning pending >>= \pending' -> loop pending' firstFailure

    _ <- launch race (primary request)
    loop (zip [1 ..] backups) Nothing

-- | Races distinct backends: the primary is launched at once and each backup after a further
-- 'hedgeDelayMs' if no attempt has ended (or straight away when an attempt fails). The first success is returned and the other attempts are cancelled, waiting until they
-- have finished. Attempts ending together are decided by their order in the list. If every attempt
-- fails, the first failure is rethrown.
hedgeAcross
  :: MonadUnliftIO m
  => HedgeOptions m SomeException b
  -- ^ Options configuring the delay and the caller's control over launches and outcomes.
  -> NonEmpty (BasicService m a b)
  -- ^ The backends, primary first.
  -> BasicService m a b
hedgeAcross = hedgeVia exceptionFailures

-- | Variant of 'hedgeAcross' for services returning failures as values: a @Left@ result is a
-- failed attempt, and the first @Left@ is returned if every attempt fails.
hedgeAcrossEither
  :: MonadUnliftIO m
  => HedgeOptions m e (Either e b)
  -- ^ Options configuring the delay and the caller's control over launches and outcomes.
  -> NonEmpty (EitherService m a e b)
  -- ^ The backends, primary first.
  -> EitherService m a e b
hedgeAcrossEither = hedgeVia valueFailures

-- | Options for configuring hedged (duplicated) service calls.
data HedgingOptions m = HedgingOptions
  { hedgingCount   :: Int
  -- ^ Total number of concurrent attempts to make (including the primary call).
  , hedgingDelayMs :: Int
  -- ^ Stagger delay in milliseconds before each subsequent hedged call (`0` to fire all simultaneously).
  , hedgingClock   :: Clock m
  -- ^ Clock used for the stagger delay.
  }

-- | Default hedging options that fire 2 concurrent requests with no stagger delay.
defaultHedgingOptions :: MonadUnliftIO m => HedgingOptions m
defaultHedgingOptions =
  HedgingOptions
    { hedgingCount   = 2
    , hedgingDelayMs = 0
    , hedgingClock   = systemClock
    }

-- | Creates a service that duplicates requests up to 'hedgingCount' times and returns
-- the first successful response, automatically cancelling remaining in-flight attempts.
-- If all attempts fail, the first encountered exception is rethrown.
hedgingService
  :: MonadUnliftIO m
  => HedgingOptions m
  -- ^ Options configuring the number of hedged calls and stagger delay.
  -> BasicService m a b
  -- ^ Underlying service to invoke.
  -> BasicService m a b
hedgingService options service
  | hedgingCount options <= 1 = service
  | otherwise =
      hedgeAcross
        defaultHedgeOptions { hedgeDelayMs = max 0 (hedgingDelayMs options), hedgeClock = hedgingClock options }
        (service :| replicate (hedgingCount options - 1) service)
