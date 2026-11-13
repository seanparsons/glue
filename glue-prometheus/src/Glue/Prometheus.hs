{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module recording Prometheus metrics for services and glue's combinators through
-- "Prometheus" (prometheus-client).
--
-- Metrics are registered once with the @register...Metrics@ functions and shared by every service
-- or combinator they instrument, each telling them apart by a caller-supplied name. Names follow
-- Prometheus conventions and every label has bounded cardinality: service, breaker and limiter
-- names are chosen by the caller, attempt indices are bounded by the number of backends, and
-- request keys are never used as labels.
--
-- [@glue_service_calls_total{service}@] Calls started.
-- [@glue_service_failures_total{service}@] Calls that failed (a synchronous exception, or @Left@).
-- [@glue_service_duration_seconds{service}@] Histogram of the duration of calls that completed.
-- [@glue_circuit_breaker_state{breaker}@] 0 closed, 1 half-open, 2 open.
-- [@glue_circuit_breaker_transitions_total{breaker,from,to}@] Phase changes (@closed@, @half_open@, @open@).
-- [@glue_retries_total{service}@] Retries made.
-- [@glue_retry_delay_seconds{service}@] Histogram of the wait chosen before each retry.
-- [@glue_concurrency_in_flight{limiter}@] Slots currently held.
-- [@glue_concurrency_refusals_total{limiter}@] Calls refused because their key was full.
-- [@glue_hedge_launches_total{service,attempt}@] Backups launched.
-- [@glue_hedge_attempts_total{service,attempt,outcome}@] Attempts ended (@answered@, @failed@, @cancelled@, @crashed@).
-- [@glue_hedge_wins_total{service,attempt}@] Answers returned, by the attempt that won.
-- [@glue_shutdown_in_flight{service}@] Calls in flight through a graceful shutdown wrapper, sampled at scrape.
module Glue.Prometheus
  ( -- * Services
    ServiceMetrics (..)
  , registerServiceMetrics
  , instrumentService
  , instrumentServiceEither
    -- * Circuit breakers
  , BreakerMetrics (..)
  , registerBreakerMetrics
  , instrumentBreakerOptions
    -- * Retries
  , RetryMetrics (..)
  , registerRetryMetrics
  , instrumentRetryOptions
    -- * Concurrency limits
  , ConcurrencyMetrics (..)
  , registerConcurrencyMetrics
  , instrumentAdmissionStore
    -- * Hedging
  , HedgeMetrics (..)
  , registerHedgeMetrics
  , instrumentHedgeOptions
    -- * Graceful shutdown
  , shutdownInFlightMetric
  ) where

import qualified Data.ByteString.Char8 as BS8
import           Data.Text             (Text, pack)
import           GHC.Clock             (getMonotonicTimeNSec)
import           Glue.CircuitBreaker   (BreakerPhase (..),
                                        BreakerTransition (..),
                                        CircuitBreakerOptions (..))
import           Glue.ConcurrencyLimit (AdmissionLease (..),
                                        AdmissionStore (..))
import           Glue.Failure          (EitherService)
import           Glue.Hedging          (AttemptOutcome (..), HedgeOptions (..))
import           Glue.Retry            (RetryOptions (..))
import           Glue.Shutdown         (ShutdownHandle (..))
import           Glue.Types            (BasicService)
import           Prometheus            (Counter, Gauge, Histogram, Info (..),
                                        Metric (..), Sample (..),
                                        SampleGroup (..), SampleType (..),
                                        Vector, addGauge, counter,
                                        defaultBuckets, gauge, histogram,
                                        incCounter, observe, register,
                                        setGauge, vector, withLabel)
import           UnliftIO              (MonadIO, MonadUnliftIO, liftIO)
import           UnliftIO.Exception    (throwIO, tryAny)

secondsSince :: MonadIO m => m (m Double)
secondsSince = do
  start <- liftIO getMonotonicTimeNSec
  pure $ do
    end <- liftIO getMonotonicTimeNSec
    pure (fromIntegral (end - start) / 1e9)

-- | Metrics for instrumented services.
data ServiceMetrics = ServiceMetrics
  { serviceCalls    :: Vector Text Counter
  , serviceFailures :: Vector Text Counter
  , serviceDuration :: Vector Text Histogram
  }

-- | Registers the service metrics with the global registry.
registerServiceMetrics :: MonadIO n => n ServiceMetrics
registerServiceMetrics =
  ServiceMetrics
    <$> register (vector "service" (counter (Info "glue_service_calls_total" "Calls started.")))
    <*> register (vector "service" (counter (Info "glue_service_failures_total" "Calls that failed.")))
    <*> register (vector "service" (histogram (Info "glue_service_duration_seconds" "Duration of calls that completed.") defaultBuckets))

instrumentVia :: MonadUnliftIO m => (r -> Bool) -> ServiceMetrics -> Text -> BasicService m a r -> BasicService m a r
instrumentVia failed metrics name service request = do
  liftIO $ withLabel (serviceCalls metrics) name incCounter
  elapsed <- secondsSince
  let finish isFailure = do
        seconds <- elapsed
        liftIO $ do
          withLabel (serviceDuration metrics) name (`observe` seconds)
          if isFailure then withLabel (serviceFailures metrics) name incCounter else pure ()
  outcome <- tryAny (service request)
  case outcome of
    Left e       -> finish True >> throwIO e
    Right result -> finish (failed result) >> pure result

-- | Records calls, failures (synchronous exceptions) and the duration of completed calls under the
-- given service name. Calls cancelled by an asynchronous exception are counted as calls only.
instrumentService :: MonadUnliftIO m => ServiceMetrics -> Text -> BasicService m a b -> BasicService m a b
instrumentService = instrumentVia (const False)

-- | Variant of 'instrumentService' for services returning failures as values, counting @Left@
-- results (as well as exceptions) as failures.
instrumentServiceEither :: MonadUnliftIO m => ServiceMetrics -> Text -> EitherService m a e b -> EitherService m a e b
instrumentServiceEither = instrumentVia (either (const True) (const False))

-- | Metrics for circuit breakers.
data BreakerMetrics = BreakerMetrics
  { breakerState       :: Vector Text Gauge
  , breakerTransitions :: Vector (Text, Text, Text) Counter
  }

-- | Registers the circuit breaker metrics with the global registry.
registerBreakerMetrics :: MonadIO n => n BreakerMetrics
registerBreakerMetrics =
  BreakerMetrics
    <$> register (vector "breaker" (gauge (Info "glue_circuit_breaker_state" "Circuit breaker phase: 0 closed, 1 half-open, 2 open.")))
    <*> register (vector ("breaker", "from", "to") (counter (Info "glue_circuit_breaker_transitions_total" "Circuit breaker phase changes.")))

phaseValue :: BreakerPhase -> Double
phaseValue BreakerClosed   = 0
phaseValue BreakerHalfOpen = 1
phaseValue BreakerOpen     = 2

phaseLabel :: BreakerPhase -> Text
phaseLabel BreakerClosed   = "closed"
phaseLabel BreakerHalfOpen = "half_open"
phaseLabel BreakerOpen     = "open"

-- | Records the breaker's phase and its transitions under the given name, keeping any existing
-- 'onBreakerTransition'. The phase gauge starts at closed.
instrumentBreakerOptions :: (MonadIO n, MonadIO m) => BreakerMetrics -> Text -> CircuitBreakerOptions m failure -> n (CircuitBreakerOptions m failure)
instrumentBreakerOptions metrics name options = do
  liftIO $ withLabel (breakerState metrics) name (`setGauge` phaseValue BreakerClosed)
  pure options
    { onBreakerTransition = \transition -> do
        liftIO $ do
          withLabel (breakerState metrics) name (`setGauge` phaseValue (transitionTo transition))
          withLabel (breakerTransitions metrics) (name, phaseLabel (transitionFrom transition), phaseLabel (transitionTo transition)) incCounter
        onBreakerTransition options transition
    }

-- | Metrics for retries.
data RetryMetrics = RetryMetrics
  { retryCount :: Vector Text Counter
  , retryDelay :: Vector Text Histogram
  }

-- | Registers the retry metrics with the global registry.
registerRetryMetrics :: MonadIO n => n RetryMetrics
registerRetryMetrics =
  RetryMetrics
    <$> register (vector "service" (counter (Info "glue_retries_total" "Retries made.")))
    <*> register (vector "service" (histogram (Info "glue_retry_delay_seconds" "Wait chosen before each retry.") defaultBuckets))

-- | Records each retry and its wait under the given service name, keeping any existing 'onRetry'.
instrumentRetryOptions :: MonadIO m => RetryMetrics -> Text -> RetryOptions m a failure -> RetryOptions m a failure
instrumentRetryOptions metrics name options =
  options
    { onRetry = \attempt failure delayMs -> do
        liftIO $ do
          withLabel (retryCount metrics) name incCounter
          withLabel (retryDelay metrics) name (`observe` (fromIntegral delayMs / 1000))
        onRetry options attempt failure delayMs
    }

-- | Metrics for keyed concurrency limits.
data ConcurrencyMetrics = ConcurrencyMetrics
  { concurrencyInFlight :: Vector Text Gauge
  , concurrencyRefusals :: Vector Text Counter
  }

-- | Registers the concurrency limit metrics with the global registry.
registerConcurrencyMetrics :: MonadIO n => n ConcurrencyMetrics
registerConcurrencyMetrics =
  ConcurrencyMetrics
    <$> register (vector "limiter" (gauge (Info "glue_concurrency_in_flight" "Slots currently held.")))
    <*> register (vector "limiter" (counter (Info "glue_concurrency_refusals_total" "Calls refused because their key was full.")))

-- | Records slots held and refusals for every key of the store under one limiter name.
instrumentAdmissionStore :: MonadIO m => ConcurrencyMetrics -> Text -> AdmissionStore m k -> AdmissionStore m k
instrumentAdmissionStore metrics name store =
  store
    { claimSlot = \key -> do
        claimed <- claimSlot store key
        case claimed of
          Nothing -> do
            liftIO $ withLabel (concurrencyRefusals metrics) name incCounter
            pure Nothing
          Just lease -> do
            liftIO $ withLabel (concurrencyInFlight metrics) name (`addGauge` 1)
            pure $ Just lease
              { releaseLease = releaseLease lease >> liftIO (withLabel (concurrencyInFlight metrics) name (`addGauge` (-1)))
              }
    }

-- | Metrics for hedged requests.
data HedgeMetrics = HedgeMetrics
  { hedgeLaunches :: Vector (Text, Text) Counter
  , hedgeAttempts :: Vector (Text, Text, Text) Counter
  , hedgeWins     :: Vector (Text, Text) Counter
  }

-- | Registers the hedging metrics with the global registry.
registerHedgeMetrics :: MonadIO n => n HedgeMetrics
registerHedgeMetrics =
  HedgeMetrics
    <$> register (vector ("service", "attempt") (counter (Info "glue_hedge_launches_total" "Backups launched.")))
    <*> register (vector ("service", "attempt", "outcome") (counter (Info "glue_hedge_attempts_total" "Attempts ended, by outcome.")))
    <*> register (vector ("service", "attempt") (counter (Info "glue_hedge_wins_total" "Answers returned, by the attempt that won.")))

outcomeLabel :: AttemptOutcome failure r -> Text
outcomeLabel (AttemptAnswered _) = "answered"
outcomeLabel (AttemptFailed _)   = "failed"
outcomeLabel AttemptCancelled    = "cancelled"
outcomeLabel (AttemptCrashed _)  = "crashed"

-- | Records backup launches, how each attempt ended and which attempt won under the given service
-- name, keeping any existing hooks.
instrumentHedgeOptions :: MonadIO m => HedgeMetrics -> Text -> HedgeOptions m failure r -> HedgeOptions m failure r
instrumentHedgeOptions metrics name options =
  options
    { admitLaunch = \index -> do
        admitted <- admitLaunch options index
        if admitted then liftIO (withLabel (hedgeLaunches metrics) (name, attemptLabel index) incCounter) else pure ()
        pure admitted
    , onAttemptEnded = \index outcome -> do
        liftIO $ withLabel (hedgeAttempts metrics) (name, attemptLabel index, outcomeLabel outcome) incCounter
        onAttemptEnded options index outcome
    , onHedgeWon = \index -> do
        liftIO $ withLabel (hedgeWins metrics) (name, attemptLabel index) incCounter
        onHedgeWon options index
    }
  where
    attemptLabel = pack . show

-- | A metric sampling the in-flight count of each named graceful shutdown wrapper at scrape time.
-- Register it once with every handle to report.
shutdownInFlightMetric :: [(Text, ShutdownHandle IO)] -> Metric ()
shutdownInFlightMetric handles = Metric $ pure ((), collect)
  where
    info = Info "glue_shutdown_in_flight" "Calls in flight through a graceful shutdown wrapper."
    collect = do
      samples <- traverse sample handles
      pure [SampleGroup info GaugeType samples]
    sample (name, handle) = do
      count <- inFlightCount handle
      pure $ Sample (metricName info) [("service", name)] (BS8.pack (show (fromIntegral count :: Double)))
