{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting failover of a service by transforming the request each time a failure occurs up to a fixed number of attempts.
module Glue.Failover(
    FailoverOptions
  , defaultFailoverOptions
  , failover
  , maxFailovers
  , transformFailoverRequest
) where

import           Control.Exception.Lifted    hiding (throw)
import           Control.Monad.Trans.Control
import           Glue.Types

-- | Options for determining behaviour of failover services.
data FailoverOptions a = FailoverOptions {
    maxFailovers             :: Int      -- ^ The maximum numer of times the service will failover and make another attempt.
  , transformFailoverRequest :: a -> a   -- ^ Each time the service performs a failover, transform the request with this function.
}

-- | Simple defaults that results in retrying 3 times in a dumb way immediately without transforming the request.
defaultFailoverOptions :: FailoverOptions a
defaultFailoverOptions = FailoverOptions { maxFailovers = 3, transformFailoverRequest = id }

-- | Creates a service that can transform requests with each subsequent failure.
failover :: (MonadBaseControl IO m)
         => FailoverOptions a     -- ^ Instance of 'FailoverOptions' to configure the failover functionality.
         -> BasicService m a b    -- ^ The service to perform failover around.
         -> BasicService m a b
failover options service =
  let invokeService failCount request =
                                        let afterFail   = (\(_ :: SomeException) -> invokeService (failCount + 1) ((transformFailoverRequest options) request))
                                            invoke      = service request
                                        in  if failCount >= (maxFailovers options) then invoke else catch invoke afterFail
  in  invokeService 0
