{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.CircuitBreakerSpec where

import           Control.Exception.Base   hiding (throw, throwIO, try)
import           Control.Exception.Lifted
import           Data.IORef
import           Data.Typeable
import           Glue.CircuitBreaker
import           Test.Hspec
import           Test.QuickCheck

data CircuitBreakerTestException = CircuitBreakerTestException deriving (Eq, Show, Typeable)
instance Exception CircuitBreakerTestException

requests :: [Int]
requests = [1..10]

spec :: Spec
spec = do
  describe "circuitBreaker" $ do
    it "Multiple failures prevent subsequent calls" $ do
      property $ \failureMax -> do
        let positiveFailureMax      = (abs failureMax) `mod` 5
        let options                 = defaultCircuitBreakerOptions { maxBreakerFailures = positiveFailureMax }
        ref                         <- newIORef (0 :: Int)
        let service _               = atomicModifyIORef' ref (\c -> (c + 1, ())) >> throwIO CircuitBreakerTestException :: IO Int
        (s, circuitBreakerService)  <- circuitBreaker options service
        results                     <- traverse (\req -> try $ try $ circuitBreakerService req) requests :: IO [Either CircuitBreakerException (Either CircuitBreakerTestException Int)]
        let expectedResults         = (replicate (positiveFailureMax + 1) (Right $ Left $ CircuitBreakerTestException)) ++ (replicate (10 - positiveFailureMax - 1) (Left $ CircuitBreakerException "Circuit breaker open."))
        results `shouldBe` expectedResults
        (isCircuitBreakerOpen s) `shouldReturn` True
        (isCircuitBreakerClosed s) `shouldReturn` False
        (readIORef ref) `shouldReturn` (positiveFailureMax + 1)
    it "Successful calls pass straight through" $ do
      property $ \(failureMax :: Int, requestIDs :: [Int]) -> do
        let positiveFailureMax      = abs failureMax
        let options                 = defaultCircuitBreakerOptions { maxBreakerFailures = positiveFailureMax }
        let service req             = return $ req + 1
        (s, circuitBreakerService)  <- circuitBreaker options service
        results                     <- traverse (\req -> circuitBreakerService req) requestIDs
        let expectedResults         = fmap (+ 1) requestIDs
        results `shouldBe` expectedResults
        (isCircuitBreakerOpen s) `shouldReturn` False
        (isCircuitBreakerClosed s) `shouldReturn` True
