{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.Failover(
    FailoverOptions
  , defaultFailoverOptions
  , failover
  , maxFailovers
  , transformFailoverRequest
) where

import Glue.Types
import Control.Exception.Lifted hiding(throw)
import Control.Monad.Trans.Control

data FailoverOptions a = FailoverOptions {
    maxFailovers              :: Int
  , transformFailoverRequest  :: a -> a
}

defaultFailoverOptions :: FailoverOptions a
defaultFailoverOptions = FailoverOptions { maxFailovers = 3, transformFailoverRequest = id }

failover :: (MonadBaseControl IO m) => FailoverOptions a -> BasicService m a b -> BasicService m a b
failover options service =
  let invokeService failCount request = 
                                        let afterFail   = (\(_ :: SomeException) -> invokeService (failCount + 1) ((transformFailoverRequest options) request))
                                            invoke      = service request
                                        in  if failCount >= (maxFailovers options) then invoke else catch invoke afterFail
  in  invokeService 0