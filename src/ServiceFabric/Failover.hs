{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ServiceFabric.Failover where

import ServiceFabric.Types
import Control.Monad.CatchIO
import Control.Exception.Base(SomeException)

data FailoverOptions a = FailoverOptions {
    maxFailovers      :: Int
  , transformRequest  :: a -> a
}

defaultFailoverOptions :: FailoverOptions a
defaultFailoverOptions = FailoverOptions { maxFailovers = 3, transformRequest = id }

failover :: (MonadCatchIO m) => FailoverOptions a -> BasicService m a b -> BasicService m a b
failover options service =
  let invokeService failCount request = 
                                        let afterFail   = (\(_ :: SomeException) -> invokeService (failCount + 1) ((transformRequest options) request))
                                            invoke      = service request
                                        in  if failCount >= (maxFailovers options) then invoke else catch invoke afterFail
  in  invokeService 0