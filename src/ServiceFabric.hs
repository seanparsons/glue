{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue
  ( module Glue.Types
  , module Glue.Failover
  , module Glue.Caching
  , module Glue.CircuitBreaker
  , module Glue.DogpileProtection
  , module Glue.Timeout
  , module Glue.Retry
  , module Glue.Batcher
) where

import Glue.Types
import Glue.Failover
import Glue.Caching
import Glue.CircuitBreaker
import Glue.DogpileProtection
import Glue.Timeout
import Glue.Retry
import Glue.Batcher

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
- Graceful service shutdown, reject new requests with a default value, wait for a period of time while existing requests continue. Then possibly a cancel action?
- Caching that supports refreshing invisibly to the consumer?
-}
