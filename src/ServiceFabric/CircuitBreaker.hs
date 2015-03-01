{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ServiceFabric.CircuitBreaker where

import Data.Typeable
import ServiceFabric.Types
import Control.Monad.IO.Class
import Data.IORef
import Data.Time.Clock.POSIX
import Control.Monad.CatchIO

data CircuitBreakerOptions = CircuitBreakerOptions {
    maxBreakerFailures  :: Int
  , resetTimeoutSecs    :: Int
  , failureDetails      :: String
}

defaultCircuitBreakerOptions :: CircuitBreakerOptions
defaultCircuitBreakerOptions = CircuitBreakerOptions { maxBreakerFailures = 3, resetTimeoutSecs = 60, failureDetails = "Circuit breaker open." }

data CircuitBreakerStatus = CircuitBreakerClosed Int | CircuitBreakerOpen Int

data CircuitBreakerException = CircuitBreakerException String deriving (Eq, Show, Typeable)
instance Exception CircuitBreakerException

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
                                      
      failingCall                 = throw $ CircuitBreakerException $ failureDetails options
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
                                        (CircuitBreakerOpen _)    -> callIfOpen request ref
                                      
  in do
        ref <- liftIO $ newIORef $ CircuitBreakerClosed 0
        return (ref, breakerService ref)
