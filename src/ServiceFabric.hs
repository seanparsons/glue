module ServiceFabric where

import ServiceFabric.Types
import Data.Cache.LRU.IO
import qualified Data.Cache.LRU.IO as LRU
import Control.Monad.IO.Class

{-
- Caching.
- Fail fast/circuit breaker.
- Multi get optimisations.
- Performance stats.
- Retries.
- Failover.
- Request duplication, with completion based on the first successful result.
-}

cacheWith :: (MonadIO m) => (a -> m b) -> (a -> b -> m ()) -> BasicService m a b -> BasicService m a b
cacheWith service lookupWith insertWith = 
  let fallback request = do
                            result <- service request
                            insertWith request result
                            return result 
      cachedService request = do
                                fromLookup <- lookupWith request
                                maybe (fallback request) return fromLookup
  in cachedService

cacheWithAtomicLRU :: (MonadIO m, Ord a) => AtomicLRU a b -> BasicService m a b -> BasicService m a b
cacheWithAtomicLRU lru service = 
  let lookupWith request            = liftIO $ LRU.lookup request lru
      insertWith request response   = liftIO $ insert request response lru
  in  cacheWith lookupWith insertWith service