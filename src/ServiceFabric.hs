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
- Performance stats.
- Request duplication, with completion based on the first successful result.
- Multi get optimisations.
-}
