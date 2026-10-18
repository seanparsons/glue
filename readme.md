Building Better Services And Clients
====================================

[![CI](https://github.com/seanparsons/glue/actions/workflows/ci.yml/badge.svg)](https://github.com/seanparsons/glue/actions/workflows/ci.yml)

`glue` provides composable combinators to handle common cross-cutting concerns of services talking to other services—such as timeouts, retries, caching, batching, circuit breaking, load balancing, concurrency limits, request hedging, and graceful shutdown—without tying your logic to a specific transport like HTTP or gRPC.

Packages
========

- **`glue-common`**: Core service types (`BasicService`, `MultiGetService`, `Call`, `MToIO`) and conversions between single-key and multi-get services.
- **`glue-core`**: Transport-agnostic resilience and performance combinators (`UnliftIO`-based):
  - `Glue.Batcher`: Time-window request batching for single and multi-get services.
  - `Glue.Caching`: LRU caching (`cacheWithLRU`, `multiCacheWithLRU`).
  - `Glue.CircuitBreaker`: Circuit breaking with failure thresholds and reset windows.
  - `Glue.ConcurrencyLimit`: Bulkhead concurrency limits (`maxInFlight` and optional `maxQueued`).
  - `Glue.DogpileProtection`: Single-flight / request coalescing to prevent cache stampedes.
  - `Glue.Failover`: Request failover with configurable request transformation.
  - `Glue.Hedging`: Request duplication / tail-latency hedging (`hedgingService`, `hedgingCall`).
  - `Glue.LoadBalancer`: Round-robin load balancing across backend services (`roundRobinLoadBalancer`, `roundRobinLoadBalancerNE`).
  - `Glue.Preload`: Background key preloading and periodic refresh.
  - `Glue.Retry`: Exponential backoff with jitter (`retryingService`, `retryingCall`).
  - `Glue.Shutdown`: Graceful service shutdown with request draining, fallback/exception rejection, and grace-period timeout (`gracefulShutdown`).
  - `Glue.Switching`: Dynamic runtime service switching (`switchingService`, `switchingCall`).
  - `Glue.Timeout`: Service call timeouts (`addTimeout`).
- **`glue-ekg`**: EKG metrics instrumentation (`System.Metrics`) for service calls, distributions, counters, and gauges.
- **`glue-example`**: Executable demonstrating batched, cached, and EKG-instrumented service composition.

Building & Development (Nix Flakes)
===================================

This repository is powered by Nix Flakes and supports building across multiple GHC versions (`ghc96`, `ghc98`, `ghc910`, `ghc912`).

```bash
# Build all packages and run test suites with the default compiler (GHC 9.10)
nix build -L

# Run all flake checks (tests + Haddock across all packages)
nix flake check -L

# Build and test against a specific GHC version
nix build -L .#ghc96-all
nix build -L .#ghc98-all
nix build -L .#ghc910-all
nix build -L .#ghc912-all

# Run the example executable
nix run .#glue-example

# Enter a development shell (includes cabal-install, hlint, fourmolu, ghcid)
nix develop
# Or for a specific GHC version:
nix develop .#ghc912
```

 You can also build directly with Cabal inside `nix develop`:

```bash
cabal build all
cabal test all
```

Examples
========

Batching
--------

In a busy service, it is often much more efficient to capture multiple individual calls over a short time window and dispatch them as a single multi-get call:

```haskell
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Glue.Batcher
import           UnliftIO.Concurrent (threadDelay)

data User = User Int String
  deriving (Eq, Show)

makeUser :: Int -> User
makeUser userId = User userId ("User " ++ show userId)

userService :: S.HashSet Int -> IO (M.HashMap Int User)
userService request = do
  threadDelay 500
  pure $ M.fromList $ fmap (\r -> (r, makeUser r)) $ S.toList request

batchedUserService :: IO (Int -> IO (Maybe User), S.HashSet Int -> IO (M.HashMap Int User), IO ())
batchedUserService = batchingService (defaultBatchingOptions id) userService
```

Retries
-------

Retry transient failures with configurable exponential backoff and jitter:

```haskell
import Glue.Retry
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef)

failingService :: IORef Int -> Int -> IO Int
failingService ref request = do
  counter <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
  if counter `mod` 3 == 0 then fail "Bang!" else pure (request * 2)

notSoFailingService :: IO (Int -> IO Int)
notSoFailingService = do
  ref <- newIORef 0
  pure $ retryingService defaultRetryOptions (failingService ref)
```

Caching
-------

Cache responses in a thread-safe LRU cache:

```haskell
import Data.Cache.LRU.IO (newAtomicLRU)
import Glue.Caching

serviceThatNeedsCaching :: Int -> IO Int
serviceThatNeedsCaching request = do
  putStrLn "Doing Something Expensive!"
  pure (request * 2)

cachedService :: IO (Int -> IO Int)
cachedService = do
  cache <- newAtomicLRU (Just 100)
  pure $ cacheWithLRU cache serviceThatNeedsCaching
```

Load Balancing, Concurrency Limits, Hedging & Graceful Shutdown
---------------------------------------------------------------

Compose resilience patterns directly around any `a -> m b` service function:

```haskell
import Data.List.NonEmpty    (NonEmpty (..))
import Glue.ConcurrencyLimit
import Glue.Hedging
import Glue.LoadBalancer
import Glue.Shutdown

resilientPipeline
  :: NonEmpty (Int -> IO String)
  -> IO (Int -> IO String, ShutdownHandle IO)
resilientPipeline backends = do
  lb      <- roundRobinLoadBalancerNE backends
  limited <- limitConcurrency (defaultConcurrencyLimitOptions { maxInFlight = 32, maxQueued = Just 64 }) lb
  let hedged = hedgingService (defaultHedgingOptions { hedgingDelayMs = 20, maxExtraRequests = 1 }) limited
  gracefulShutdown defaultShutdownOptions hedged
```