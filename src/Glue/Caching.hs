{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.Caching(
    cacheWithBasic
  , cacheWithMulti
) where

import Data.Hashable
import Glue.Types
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

-- |Values within m will be lost for calls that hit the cache.
cacheWithBasic :: (Monad m) => (a -> m (Maybe b)) -> (a -> b -> m ()) -> BasicService m a b -> BasicService m a b
cacheWithBasic lookupWith insertWith service = 
  let fallback request = do
                            result <- service request
                            insertWith request result
                            return result 
      cachedService request = do
                                fromCache <- lookupWith request
                                maybe (fallback request) return fromCache
  in cachedService

-- |Values within m will be lost for calls that hit the cache.
cacheWithMulti :: (Monad m, Functor m, Eq a, Hashable a) => ((MultiGetRequest a) -> m (MultiGetResponse a b)) -> ((MultiGetResponse a b) -> m ()) -> MultiGetService m a b -> MultiGetService m a b
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
