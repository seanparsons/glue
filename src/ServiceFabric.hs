{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ServiceFabric
  ( module ServiceFabric.Types
  , module ServiceFabric.Failover
  , module ServiceFabric.Caching
  , module ServiceFabric.CircuitBreaker
  , module ServiceFabric.DogpileProtection
  , module ServiceFabric.Timeout
) where

import ServiceFabric.Types
import ServiceFabric.Failover
import ServiceFabric.Caching
import ServiceFabric.CircuitBreaker
import ServiceFabric.DogpileProtection
import ServiceFabric.Timeout

{-
- Caching.
- Fail fast/circuit breaker.
- Failover.
- Dogpile protection.
- Timeouts.

- Retries.
- Performance stats.
- Multi get optimisations.
- Request duplication, with completion based on the first successful result.
-}
