{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | Module containing circuit breaker functionality, which is the ability to open a circuit once a number of failures have occurred, thereby preventing later calls from attempting to make unsuccessful calls.
-- | Often this is useful if the underlying service were to repeatedly time out, so as to reduce the number of calls inflight holding up upstream callers.
module Glue.CircuitBreaker(
    CircuitBreakerOptions
  , CircuitBreakerStatus
  , CircuitBreakerException(..)
  , defaultCircuitBreakerOptions
  , circuitBreaker
  , maxBreakerFailures
  , resetTimeoutSecs
  , breakerDescription
) where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Trans.Control
import Data.IORef.Lifted
import Data.Time.Clock.POSIX
import Data.Typeable
import Glue.Types

-- | Options for determining behaviour of circuit breaking services.
data CircuitBreakerOptions = CircuitBreakerOptions {
    maxBreakerFailures  :: Int        -- ^ How many times the underlying service must fail in the given window before the circuit opens.
  , resetTimeoutSecs    :: Int        -- ^ The window of time in which the underlying service must fail for the circuit to open.
  , breakerDescription  :: String     -- ^ Description that is attached to the failure so as to identify the particular circuit.
}

-- | Defaulted options for the circuit breaker with 3 failures over 60 seconds.
defaultCircuitBreakerOptions :: CircuitBreakerOptions
defaultCircuitBreakerOptions = CircuitBreakerOptions { maxBreakerFailures = 3, resetTimeoutSecs = 60, breakerDescription = "Circuit breaker open." }

data CircuitBreakerStatus = CircuitBreakerClosed Int | CircuitBreakerOpen Int

data CircuitBreakerException = CircuitBreakerException String deriving (Eq, Show, Typeable)
instance Exception CircuitBreakerException

-- TODO: Check that values within m aren't lost on a successful call.
-- | Circuit breaking services can be constructed with this function.
circuitBreaker :: (MonadBaseControl IO m, MonadBaseControl IO n) => CircuitBreakerOptions -> BasicService m a b -> n (IORef CircuitBreakerStatus, BasicService m a b)
circuitBreaker options service = 
  let getCurrentTime              = liftBase $ round `fmap` getPOSIXTime
      failureMax                  = maxBreakerFailures options
      callIfClosed request ref    = bracketOnError (return ()) (\_ -> incErrors ref) (\_ -> service request)
      canaryCall request ref      = do
                                      result <- callIfClosed request ref
                                      writeIORef ref $ CircuitBreakerClosed 0
                                      return result
      incErrors ref               = do
                                      currentTime <- getCurrentTime
                                      atomicModifyIORef' ref $ \status -> case status of
                                        (CircuitBreakerClosed errorCount) -> (if errorCount >= failureMax then CircuitBreakerOpen (currentTime + (resetTimeoutSecs options)) else CircuitBreakerClosed (errorCount + 1), ())
                                        other                             -> (other, ())
                                      
      failingCall                 = throw $ CircuitBreakerException $ breakerDescription options
      callIfOpen request ref      = do
                                      currentTime <- getCurrentTime
                                      canaryRequest <- atomicModifyIORef' ref $ \status -> case status of 
                                                              (CircuitBreakerClosed _)  -> (status, False)
                                                              (CircuitBreakerOpen time) -> if currentTime > time then ((CircuitBreakerOpen (currentTime + (resetTimeoutSecs options))), True) else (status, False)
                                      
                                      if canaryRequest then canaryCall request ref else failingCall
      breakerService ref request  = do
                                      status <- readIORef ref
                                      case status of 
                                        (CircuitBreakerClosed _)  -> callIfClosed request ref
                                        (CircuitBreakerOpen _)    -> callIfOpen request ref
                                      
  in do
        ref <- newIORef $ CircuitBreakerClosed 0
        return (ref, breakerService ref)
