{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module ServiceFabric where

import ServiceFabric.Types
import Data.Cache.LRU.IO
import qualified Data.Cache.LRU.IO as LRU
import Control.Monad.IO.Class
import Data.IORef
import Data.Time.Clock.POSIX
import Control.Monad.CatchIO
import Prelude hiding (catch)
import Control.Exception.Base(SomeException)

{-
- Caching.
- Timeouts.
- Fail fast/circuit breaker.
- Multi get optimisations.
- Performance stats.
- Retries.
- Failover.
- Dogpiling protection.
- Request duplication, with completion based on the first successful result.
-}

data FailoverOptions a = FailoverOptions {
    maxFailovers      :: Int
  , transformRequest  :: a -> a
}

defaultFailoverOptions = FailoverOptions { maxFailovers = 3, transformRequest = id }

failover :: (MonadCatchIO m) => FailoverOptions a -> BasicService m a b -> BasicService m a b
failover options service =
  let invokeService failCount request = 
                                        let afterFail   = (\(e :: SomeException) -> invokeService (failCount + 1) ((transformRequest options) request))
                                            invoke      = service request
                                        in  if failCount > (maxFailovers options) then invoke else catch invoke afterFail
  in  invokeService 0

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

cacheWithAtomicLRU :: (MonadIO m, Ord a) => AtomicLRU a b -> BasicService m a b -> BasicService m a b
cacheWithAtomicLRU lru service = 
  let lookupWith request            = liftIO $ LRU.lookup request lru
      insertWith request response   = liftIO $ insert request response lru
  in  cacheWith lookupWith insertWith service

data CircuitBreakerOptions = CircuitBreakerOptions {
    maxBreakerFailures  :: Int
  , resetTimeoutSecs    :: Int
  , failureDetails      :: String
}

defaultCircuitBreakerOptions = CircuitBreakerOptions { maxBreakerFailures = 3, resetTimeoutSecs = 60, failureDetails = "Circuit breaker open." }

data CircuitBreakerStatus = CircuitBreakerClosed Int | CircuitBreakerOpen Int

circuitBreaker :: (MonadCatchIO m) => CircuitBreakerOptions -> BasicService m a b -> m (IORef CircuitBreakerStatus, BasicService m a b)
circuitBreaker options service = 
  let getCurrentTime              = liftIO $ round `fmap` getPOSIXTime
      failureMax                  = maxBreakerFailures options
      callIfClosed request ref    = bracketOnError (return ()) (\_ -> incErrors ref) (\_ -> service request)
      canaryCall request ref      = do
                                      result <- callIfClosed request ref
                                      liftIO $ writeIORef ref $ CircuitBreakerClosed 0
                                      return result
      incErrors ref               = do
                                      currentTime <- getCurrentTime
                                      liftIO $ atomicModifyIORef' ref $ \status -> case status of
                                        (CircuitBreakerClosed errorCount) -> (if errorCount >= failureMax then CircuitBreakerOpen (currentTime + (resetTimeoutSecs options)) else CircuitBreakerClosed (errorCount + 1), ())
                                        other                             -> (other, ())
                                      
      failingCall                 = fail $ failureDetails options
      callIfOpen request ref      = do
                                      currentTime <- getCurrentTime
                                      canaryRequest <- liftIO $ atomicModifyIORef' ref $ \status -> case status of 
                                                              (CircuitBreakerClosed _)  -> (status, False)
                                                              (CircuitBreakerOpen time) -> if currentTime > time then ((CircuitBreakerOpen (currentTime + (resetTimeoutSecs options))), True) else (status, False)
                                      
                                      if canaryRequest then canaryCall request ref else failingCall
      breakerService ref request  = do
                                      status <- liftIO $ readIORef ref
                                      case status of 
                                        (CircuitBreakerClosed _)  -> callIfClosed request ref
                                        (CircuitBreakerOpen time) -> callIfOpen request ref
                                      
  in do
        ref <- liftIO $ newIORef $ CircuitBreakerClosed 0
        return (ref, breakerService ref)
