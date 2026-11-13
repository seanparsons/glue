# Changelog for glue-core

## 0.8.0.0

### Breaking changes

- Options records for time-dependent combinators take the monad as a type parameter and carry a
  `Clock m` defaulting to `systemClock`: `TimeoutOptions m`, `RetryOptions m a`,
  `HedgingOptions m`, `BatchingOptions m`, `DogpileOptions m`, `ShutdownOptions m b` and
  `PreloadedOptions` (new `preloadingClock` field). Code that updates fields of the defaults keeps
  compiling; when an update replaces every field that mentions `m` (for example only the clock),
  annotate the default, e.g. `(defaultTimeoutOptions :: TimeoutOptions IO) { timeoutClock = c }` (#13).

### Additions

- `addTimeoutEither` and `gracefulShutdownEither` return their refusal as a value for services
  whose failures are values (#6).

## 0.7.0.1

- PVP upper bounds on all dependencies.

## 0.7.0

- Modernised for GHC 9.6 to 9.12, `MonadUnliftIO` throughout, new combinators (hedging,
  concurrency limits, load balancing, graceful shutdown) and a Nix flake.
