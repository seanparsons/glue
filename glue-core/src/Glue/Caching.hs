{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module supporting the caching of a service.
module Glue.Caching(
    cacheWithBasic
  , cacheWithMulti
) where

import           Data.Hashable
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet        as S
import           Glue.Types

-- | Caching of a `BasicService` instance, that defers to external functions for the actual caching.
-- | Note: Values within m will be lost for calls that hit the cache.
cacheWithBasic :: (Monad m)
               => (a -> m (Maybe b))  -- ^ Cache lookup function, used before potentially invoking the fallback service.
               -> (a -> b -> m ())    -- ^ Cache write function, used after invoking the fallback service to populate the cache.
               -> BasicService m a b  -- ^ The service to cache.
               -> BasicService m a b
cacheWithBasic lookupWith insertWith service =
  let fallback request = do
                            result <- service request
                            insertWith request result
                            return result
      cachedService request = do
                                fromCache <- lookupWith request
                                maybe (fallback request) return fromCache
  in cachedService

-- | Caching of a `MultiGetService` instance, that defers to external functions for the actual caching.
-- | Partial responses will result in partial fallback calls that get just the missing keys.
-- | Values within m will be lost for calls that hit the cache.
cacheWithMulti :: (Monad m, Functor m, Eq a, Hashable a)
               => ((MultiGetRequest a) -> m (MultiGetResponse a b)) -- ^ Cache lookup function, used before potentially invoking the fallback service.
               -> ((MultiGetResponse a b) -> m ())                  -- ^ Cache write function, used after invoking the fallback service to populate the cache.
               -> MultiGetService m a b                             -- ^ The service to cache.
               -> MultiGetService m a b
cacheWithMulti lookupWith insertWith service =
  let fallback request = do
                            result <- service request
                            insertWith result
                            return result
      cachedService request = do
                                fromCache <- lookupWith request
                                let uncachedKeys = S.difference request (S.fromList $ M.keys fromCache)
                                if (S.null uncachedKeys) then (return fromCache) else (fmap (M.union fromCache) $ fallback uncachedKeys)
  in cachedService
