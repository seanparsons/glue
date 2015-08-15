{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module that re-exports the entirety of the library.
module Glue
  ( module Glue.Types
  , module Glue.Failover
  , module Glue.Caching
  , module Glue.CircuitBreaker
  , module Glue.DogpileProtection
  , module Glue.Timeout
  , module Glue.Retry
  , module Glue.Batcher
  , module Glue.Stats
  , module Glue.Preload
) where

import Glue.Types
import Glue.Failover
import Glue.Caching
import Glue.CircuitBreaker
import Glue.DogpileProtection
import Glue.Timeout
import Glue.Retry
import Glue.Batcher
import Glue.Stats
import Glue.Preload

{-
Done:
- Caching.
- Fail fast/circuit breaker.
- Failover.
- Dogpile protection.
- Timeouts.
- Retries.
- Multi-get optimisations.
- Performance stats.

Todo:
- Load balancer, maybe just a simple round robin to start with.
- Concurrent request constraint, possibly X inflight and Y queued.
- Switching service, allowing different implementations to be switched in and out at runtime.
- Routing service? Possibly something akin to HTTP routing in a lot of web frameworks.
- Request duplication, with completion based on the first successful result.
- Graceful service shutdown, reject new requests with a default value, wait for a period of time while existing requests continue. Then possibly a cancel action?
- Caching that supports refreshing invisibly to the consumer?
-}
