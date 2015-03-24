{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.Caching(
    cacheWith
) where

import Glue.Types
import Control.Monad.IO.Class

-- Values within m will be lost for calls that hit the cache.
cacheWith :: (MonadIO m) => (a -> m (Maybe b)) -> (a -> b -> m ()) -> BasicService m a b -> BasicService m a b
cacheWith lookupWith insertWith service = 
  let fallback request = do
                            result <- service request
                            insertWith request result
                            return result 
      cachedService request = do
                                fromLookup <- lookupWith request
                                maybe (fallback request) return fromLookup
  in cachedService
