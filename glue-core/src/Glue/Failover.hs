{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting failover of a service by transforming the request each time a failure occurs up to a fixed number of attempts.
module Glue.Failover
  ( FailoverOptions (..)
  , defaultFailoverOptions
  , failover
  ) where

import           Glue.Types
import           UnliftIO           (MonadUnliftIO)
import           UnliftIO.Exception (catchAny)

-- | Options for determining behaviour of failover services.
data FailoverOptions a = FailoverOptions
  { maxFailovers             :: Int
  -- ^ The maximum number of times the service will failover and make another attempt.
  , transformFailoverRequest :: a -> a
  -- ^ Each time the service performs a failover, transform the request with this function.
  }

-- | Simple defaults that result in retrying 3 times immediately without transforming the request.
defaultFailoverOptions :: FailoverOptions a
defaultFailoverOptions =
  FailoverOptions
    { maxFailovers             = 3
    , transformFailoverRequest = id
    }

-- | Creates a service that can transform requests with each subsequent failure.
failover
  :: MonadUnliftIO m
  => FailoverOptions a
  -- ^ Instance of t'FailoverOptions' to configure the failover functionality.
  -> BasicService m a b
  -- ^ The service to perform failover around.
  -> BasicService m a b
failover options service =
  let invokeService failCount request =
        let afterFail _ = invokeService (failCount + 1) (transformFailoverRequest options request)
            invoke      = service request
        in  if failCount >= maxFailovers options
              then invoke
              else catchAny invoke afterFail
  in  invokeService 0
