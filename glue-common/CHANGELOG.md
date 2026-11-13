# Changelog for glue-common

## 0.8.0.0

- Added `Glue.Clock`: an injectable `Clock` (monotonic `clockNow` in microseconds, `clockSleep`,
  `clockTimeout`) with `systemClock` as the real implementation, plus `remainingUntil` for
  deadline-aware code (#13).
- Added `Glue.Clock.Virtual`: a virtual clock for deterministic tests that only moves when
  `advanceClock` is called and wakes sleepers in deadline order (#13).
- Added `Glue.Failure`: `EitherService` for services that return failures as values, and
  `FailureMode` (`exceptionFailures`, `valueFailures`) so combinators can support both shapes,
  plus `ExceptT` conversions (#6).

## 0.7.0.1

- PVP upper bounds on all dependencies.

## 0.7.0

- Modernised for GHC 9.6 to 9.12, `MonadUnliftIO` throughout and a Nix flake.
