{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module containing circuit breaker functionality, which is the ability to open a circuit
-- once a number of failures have occurred, thereby preventing later calls from attempting
-- to make unsuccessful calls.
-- Often this is useful if the underlying service were to repeatedly time out, so as to
-- reduce the number of calls inflight holding up upstream callers.
--
-- The breaker is closed until 'breakerFailureThreshold' counted failures have occurred (consecutive
-- failures by default, or failures within 'breakerFailureWindowMs'), then open for
-- 'breakerOpenDurationMs', refusing calls. After that the next call is admitted as the single
-- half-open trial: if it answers the breaker closes, if it fails the breaker opens again.
--
-- Only synchronous failures accepted by 'countsAsFailure' count. A call cancelled by an
-- asynchronous exception (a timeout, a losing hedge, a killed request thread) counts for nothing,
-- and the exception is rethrown.
module Glue.CircuitBreaker
  ( CircuitBreakerOptions (..)
  , defaultCircuitBreakerOptions
  , BreakerPhase (..)
  , BreakerTransition (..)
  , Admission (..)
  , Settlement (..)
  , BreakerStore (..)
  , newInMemoryBreakerStore
  , CircuitBreakerState
  , CircuitBreakerException (..)
  , combineCircuitBreakerStates
  , circuitBreakerPhases
  , isCircuitBreakerOpen
  , isCircuitBreakerClosed
  , circuitBreaker
  , circuitBreakerEither
  , circuitBreakerWithStore
  , circuitBreakerEitherWithStore
  ) where

import           Data.Foldable      (traverse_)
import           Data.Int           (Int64)
import           Glue.Clock
import           Glue.Failure
import           Glue.Types
import           UnliftIO           (Exception, MonadIO, MonadUnliftIO,
                                     SomeException)
import           UnliftIO.Exception (mask, onException, throwIO)
import           UnliftIO.IORef     (atomicModifyIORef', newIORef, readIORef)

-- | Options for determining behaviour of circuit breaking services.
data CircuitBreakerOptions m failure = CircuitBreakerOptions
  { breakerFailureThreshold :: Int
  -- ^ The circuit opens on this many counted failures, so @3@ opens on the third (minimum 1).
  , breakerFailureWindowMs  :: Maybe Int
  -- ^ 'Nothing' counts consecutive failures, a call that answers resetting the count.
  -- @'Just' w@ counts the failures within the last @w@ milliseconds instead.
  , breakerOpenDurationMs   :: Int
  -- ^ How long (in milliseconds) the circuit stays open before a single trial call is admitted.
  , breakerDescription      :: String
  -- ^ Description that is attached to the failure so as to identify the particular circuit.
  , countsAsFailure         :: failure -> Bool
  -- ^ Whether a failure says something about the backend's health. Failures that don't count
  -- (a caller's own bad request, say) are treated as the backend having answered.
  , onBreakerTransition     :: BreakerTransition -> m ()
  -- ^ Called after each change of phase, for metrics and alerts.
  , breakerClock            :: Clock m
  -- ^ Clock used for the failure window and the open duration.
  }

-- | Defaulted options for the circuit breaker: opens on the 3rd consecutive failure, for 60 seconds,
-- counting every failure.
defaultCircuitBreakerOptions :: MonadUnliftIO m => CircuitBreakerOptions m failure
defaultCircuitBreakerOptions =
  CircuitBreakerOptions
    { breakerFailureThreshold = 3
    , breakerFailureWindowMs  = Nothing
    , breakerOpenDurationMs   = 60000
    , breakerDescription      = "Circuit breaker open."
    , countsAsFailure         = const True
    , onBreakerTransition     = const (pure ())
    , breakerClock            = systemClock
    }

-- | The phase a circuit breaker is in.
data BreakerPhase
  = BreakerClosed
  -- ^ Calls pass through.
  | BreakerOpen
  -- ^ Calls are refused.
  | BreakerHalfOpen
  -- ^ A single trial call is in flight; other calls are refused.
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A change of phase.
data BreakerTransition = BreakerTransition
  { transitionFrom :: BreakerPhase
  , transitionTo   :: BreakerPhase
  }
  deriving (Eq, Show)

-- | The store's decision about a call.
data Admission
  = AdmittedClosed
  -- ^ The circuit is closed and the call may proceed.
  | AdmittedTrial
  -- ^ The call is the single half-open trial.
  | Refused
  -- ^ The circuit is open (or a trial is in flight) and the call must not be made.
  deriving (Eq, Show)

-- | How an admitted call ended, as far as the breaker is concerned.
data Settlement
  = SettledAnswered
  -- ^ The backend answered: a success, or a failure that 'countsAsFailure' rejected.
  | SettledFailed
  -- ^ A counted failure.
  | SettledAbandoned
  -- ^ The call was cancelled (or threw outside the failure mode) and says nothing about health.
  deriving (Eq, Show)

-- | The state behind a circuit breaker, abstracted so that it can be shared between instances of a
-- horizontally scaled service (a database row updated with one conditional statement using the
-- database clock, say). Implementations must make 'admitCall' atomic so only one caller is ever
-- given 'AdmittedTrial' for a given half-open period.
data BreakerStore m = BreakerStore
  { admitCall  :: m (Admission, Maybe BreakerTransition)
  -- ^ Decide whether a call may proceed, reporting any phase change this caused.
  , settleCall :: Admission -> Settlement -> m (Maybe BreakerTransition)
  -- ^ Record how an admitted call ended, reporting any phase change this caused.
  , readPhase  :: m BreakerPhase
  -- ^ The current phase, for display.
  }

data MemoryState
  = MemoryClosed [Int64]
  -- ^ Times of the counted failures still in play, newest first.
  | MemoryOpen Int64
  -- ^ Open until the given time.
  | MemoryHalfOpen

-- | The in-memory t'BreakerStore' used by 'circuitBreaker', configured from the threshold, window,
-- open duration and clock in the options.
newInMemoryBreakerStore :: (MonadIO m, MonadIO n) => CircuitBreakerOptions m failure -> n (BreakerStore m)
newInMemoryBreakerStore options = do
  ref <- newIORef (MemoryClosed [])
  let clock       = breakerClock options
      threshold   = max 1 (breakerFailureThreshold options)
      openUs      = fromIntegral (max 0 (breakerOpenDurationMs options)) * 1000
      windowUs    = fmap (\w -> fromIntegral (max 0 w) * 1000) (breakerFailureWindowMs options)
      inWindow now = maybe id (\w -> takeWhile (> now - w)) windowUs
      transition from to = Just (BreakerTransition from to)
      admit now state = case state of
        MemoryClosed _ -> (state, (AdmittedClosed, Nothing))
        MemoryOpen until'
          | now >= until' -> (MemoryHalfOpen, (AdmittedTrial, transition BreakerOpen BreakerHalfOpen))
          | otherwise     -> (state, (Refused, Nothing))
        MemoryHalfOpen -> (state, (Refused, Nothing))
      settle now AdmittedClosed settlement (MemoryClosed failures) = case settlement of
        SettledAnswered
          | Just _ <- windowUs -> (MemoryClosed (inWindow now failures), Nothing)
          | otherwise          -> (MemoryClosed [], Nothing)
        SettledFailed ->
          let failures' = now : inWindow now failures
          in  if length failures' >= threshold
                then (MemoryOpen (now + openUs), transition BreakerClosed BreakerOpen)
                else (MemoryClosed failures', Nothing)
        SettledAbandoned -> (MemoryClosed failures, Nothing)
      settle now AdmittedTrial settlement MemoryHalfOpen = case settlement of
        SettledAnswered  -> (MemoryClosed [], transition BreakerHalfOpen BreakerClosed)
        SettledFailed    -> (MemoryOpen (now + openUs), transition BreakerHalfOpen BreakerOpen)
        -- The trial told us nothing, so the next call may try again straight away.
        SettledAbandoned -> (MemoryOpen now, transition BreakerHalfOpen BreakerOpen)
      settle _ _ _ state = (state, Nothing)
      phaseOf (MemoryClosed _) = BreakerClosed
      phaseOf (MemoryOpen _)   = BreakerOpen
      phaseOf MemoryHalfOpen   = BreakerHalfOpen
  pure
    BreakerStore
      { admitCall  = do
          now <- clockNow clock
          atomicModifyIORef' ref (admit now)
      , settleCall = \admission settlement -> do
          now <- clockNow clock
          atomicModifyIORef' ref (settle now admission settlement)
      , readPhase  = phaseOf <$> readIORef ref
      }

-- | Representation of the state the circuit breaker is currently in.
newtype CircuitBreakerState m = CircuitBreakerState [m BreakerPhase]

-- | Exception thrown when the circuit is open.
newtype CircuitBreakerException = CircuitBreakerException String
  deriving (Eq, Show)

instance Exception CircuitBreakerException

-- | Combines multiple states together.
combineCircuitBreakerStates :: Foldable t => t (CircuitBreakerState m) -> CircuitBreakerState m
combineCircuitBreakerStates = CircuitBreakerState . foldMap (\(CircuitBreakerState phases) -> phases)

-- | The phases of each breaker in the state.
circuitBreakerPhases :: Applicative m => CircuitBreakerState m -> m [BreakerPhase]
circuitBreakerPhases (CircuitBreakerState phases) = sequenceA phases

-- | Determines if a circuit breaker is open (or half-open) in any of its breakers.
isCircuitBreakerOpen :: Applicative m => CircuitBreakerState m -> m Bool
isCircuitBreakerOpen state = any (/= BreakerClosed) <$> circuitBreakerPhases state

-- | Determines if a circuit breaker is closed in all of its breakers.
isCircuitBreakerClosed :: Applicative m => CircuitBreakerState m -> m Bool
isCircuitBreakerClosed state = all (== BreakerClosed) <$> circuitBreakerPhases state

breakerVia
  :: MonadUnliftIO m
  => FailureMode m r failure
  -> m r
  -> CircuitBreakerOptions m failure
  -> BreakerStore m
  -> BasicService m a r
  -> BasicService m a r
breakerVia mode refuse options store service request =
  mask $ \restore -> do
    let notify = traverse_ (onBreakerTransition options)
    (admission, admitted) <- admitCall store
    case admission of
      Refused -> restore (notify admitted >> refuse)
      _ -> do
        outcome <-
          restore (notify admitted >> observeCall mode (service request))
            `onException` (settleCall store admission SettledAbandoned >>= notify)
        let settlement = either (\f -> if countsAsFailure options f then SettledFailed else SettledAnswered) (const SettledAnswered) outcome
        settled <- settleCall store admission settlement
        restore $ do
          notify settled
          either (reraise mode) pure outcome

-- | Circuit breaking services can be constructed with this function.
circuitBreaker
  :: (MonadUnliftIO m, MonadIO n)
  => CircuitBreakerOptions m SomeException
  -- ^ Options for specifying the circuit breaker behaviour.
  -> BasicService m a b
  -- ^ Service to protect with the circuit breaker.
  -> n (CircuitBreakerState m, BasicService m a b)
circuitBreaker options service = do
  store <- newInMemoryBreakerStore options
  pure (CircuitBreakerState [readPhase store], circuitBreakerWithStore options store service)

-- | Variant of 'circuitBreaker' using the given store, for state shared between instances.
-- The threshold, window and open duration in the options are the store's concern and unused here.
circuitBreakerWithStore
  :: MonadUnliftIO m
  => CircuitBreakerOptions m SomeException
  -- ^ Options for specifying the circuit breaker behaviour.
  -> BreakerStore m
  -- ^ Store holding the breaker's state.
  -> BasicService m a b
  -- ^ Service to protect with the circuit breaker.
  -> BasicService m a b
circuitBreakerWithStore options =
  breakerVia exceptionFailures (throwIO $ CircuitBreakerException $ breakerDescription options) options

-- | Variant of 'circuitBreaker' for services returning failures as values: @Left@ results are
-- classified by 'countsAsFailure', and a refused call returns the given failure instead of throwing.
circuitBreakerEither
  :: (MonadUnliftIO m, MonadIO n)
  => CircuitBreakerOptions m e
  -- ^ Options for specifying the circuit breaker behaviour ('breakerDescription' is unused).
  -> e
  -- ^ Failure returned when the circuit is open.
  -> EitherService m a e b
  -- ^ Service to protect with the circuit breaker.
  -> n (CircuitBreakerState m, EitherService m a e b)
circuitBreakerEither options onBreakerOpen service = do
  store <- newInMemoryBreakerStore options
  pure (CircuitBreakerState [readPhase store], circuitBreakerEitherWithStore options store onBreakerOpen service)

-- | Variant of 'circuitBreakerEither' using the given store.
circuitBreakerEitherWithStore
  :: MonadUnliftIO m
  => CircuitBreakerOptions m e
  -- ^ Options for specifying the circuit breaker behaviour.
  -> BreakerStore m
  -- ^ Store holding the breaker's state.
  -> e
  -- ^ Failure returned when the circuit is open.
  -> EitherService m a e b
  -- ^ Service to protect with the circuit breaker.
  -> EitherService m a e b
circuitBreakerEitherWithStore options store onBreakerOpen =
  breakerVia valueFailures (pure (Left onBreakerOpen)) options store
