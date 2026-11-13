# Changelog for glue-core

## 0.8.0.0

### Breaking changes

- Options records for time-dependent combinators take the monad as a type parameter and carry a
  `Clock m` defaulting to `systemClock`: `TimeoutOptions m`, `RetryOptions m a`,
  `HedgingOptions m`, `BatchingOptions m`, `DogpileOptions m`, `ShutdownOptions m b` and
  `PreloadedOptions` (new `preloadingClock` field). Code that updates fields of the defaults keeps
  compiling; when an update replaces every field that mentions `m` (for example only the clock),
  annotate the default, e.g. `(defaultTimeoutOptions :: TimeoutOptions IO) { timeoutClock = c }` (#13).
- `Glue.CircuitBreaker` counting is fixed and its semantics match the documentation (#5):
  - Calls cancelled by an asynchronous exception (timeouts, losing hedges, killed threads) no
    longer count as failures; the exception is still rethrown.
  - `maxBreakerFailures` is replaced by `breakerFailureThreshold`, which opens on that failure
    (the default 3 opens on the third; previously the breaker opened on failure `max + 1`).
  - Failures are consecutive by default (a call that answers resets the count; previously the
    count never reset), or counted within `breakerFailureWindowMs` when set.
  - `resetTimeoutSecs` is replaced by `breakerOpenDurationMs`, how long the breaker stays open,
    with millisecond resolution on the monotonic clock rather than wall-clock seconds.
- `CircuitBreakerOptions` and `CircuitBreakerState` take the monad (and options the failure type)
  as type parameters.
- `RetryOptions m a failure`: `retryInitialWaitTimeMs` and `retryWaitTimeMultiplier` are replaced
  by `retryBackoff` (`ConstantBackoff`, `LinearBackoff` or `ExponentialBackoff`) (#7).

### Additions

- Circuit breaker failure classification with `countsAsFailure`, a phase observer
  `onBreakerTransition`, `circuitBreakerPhases`, and a pluggable `BreakerStore` (admit / settle /
  read phase, with an atomic half-open trial) so state can be shared between instances, with
  `newInMemoryBreakerStore` as the default (#9).
- `circuitBreakerEither` and `circuitBreakerEitherWithStore` for services returning failures as
  values (#6).
- Retry decided by the failure (`retryOn`), with a delay ceiling (`retryMaxDelayMs`), full or equal
  jitter from an injectable random source (`retryJitter`, `retryRandom`), a deadline after which no
  retry starts (`retryRemainingMs`), an attempt observer (`onRetry`) and `retryingServiceEither` (#7).
- `failoverAcross` tries an ordered list of distinct backends, with `advanceOn` deciding which
  failures move on and which stop the chain, a choice of reporting the first or last failure, an
  indexed variant reporting which backend answered, and `Either` variants (#8).
- `Glue.Race`: a low-level race of attempts (`withRace`, `launch`, `awaitNext` with an optional
  timeout, `abandon`, `abandonAll`) for callers driving their own launch decisions. Simultaneous
  endings are reported earliest index first, and abandoning waits until the attempt has finished,
  reporting how it really ended (#10).
- `hedgeAcross` hedges across distinct backends on top of `Glue.Race`: `admitLaunch` lets the
  caller decline a backup without failing the primary, `onAttemptEnded` reports every attempt
  (including cancelled losers), an early failure launches the next backend at once, and
  `hedgeAcrossEither` supports failures as values. `hedgingService` is now built on it (#10).
- `addTimeoutEither` and `gracefulShutdownEither` return their refusal as a value for services
  whose failures are values (#6).

## 0.7.0.1

- PVP upper bounds on all dependencies.

## 0.7.0

- Modernised for GHC 9.6 to 9.12, `MonadUnliftIO` throughout, new combinators (hedging,
  concurrency limits, load balancing, graceful shutdown) and a Nix flake.
