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
  , module Glue.Stats
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

{-
- Caching.
- Fail fast/circuit breaker.
- Failover.
- Dogpile protection.
- Timeouts.
- Retries.
- Multi-get optimisations.

- Performance stats, T-Digest maybe? Doesn't appear that the statistics package supports percentiles, but it's hard to tell.
  Or we should just shunt this into the capable hands of ekg.
- Request duplication, with completion based on the first successful result.
- Graceful service shutdown, reject new requests with a default value, wait for a period of time while existing requests continue. Then possibly a cancel action?
- Caching that supports refreshing invisibly to the consumer?
-}
