{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ServiceFabric
  ( module ServiceFabric.Types
  , module ServiceFabric.Failover
  , module ServiceFabric.Caching
  , module ServiceFabric.CircuitBreaker
  , module ServiceFabric.DogpileProtection
) where

import ServiceFabric.Types
import ServiceFabric.Failover
import ServiceFabric.Caching
import ServiceFabric.CircuitBreaker
import ServiceFabric.DogpileProtection

{-
- Caching.
- Fail fast/circuit breaker.
- Failover.
- Dogpile protection.

- Timeouts.
- Multi get optimisations.
- Performance stats.
- Retries.
- Request duplication, with completion based on the first successful result.
-}
