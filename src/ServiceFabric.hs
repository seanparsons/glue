{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServiceFabric
  ( module ServiceFabric.Types
  , module ServiceFabric.Failover
  , module ServiceFabric.Caching
  , module ServiceFabric.CircuitBreaker
  , module ServiceFabric.DogpileProtection
  , module ServiceFabric.Timeout
  , module ServiceFabric.Retry
) where

import ServiceFabric.Types
import ServiceFabric.Failover
import ServiceFabric.Caching
import ServiceFabric.CircuitBreaker
import ServiceFabric.DogpileProtection
import ServiceFabric.Timeout
import ServiceFabric.Retry

{-
- Caching.
- Fail fast/circuit breaker.
- Failover.
- Dogpile protection.
- Timeouts.
- Retries.

- Multi-get optimisations.
  - Multi-get combining with multi-get dogpile protection will deal with 
    smoothing multiple requests from different sources requesting the same thing.
  - Potentially instead of trying to do complex combining of lookups, 
    dispatch a multi-get query every X ms, which is the accumulation of all the requests
    made.
- Performance stats.
- Request duplication, with completion based on the first successful result.
-}
