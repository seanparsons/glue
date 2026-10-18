{-# LANGUAGE ScopedTypeVariables #-}

module Glue.CircuitBreakerSpec (spec) where

import           Data.IORef
import           Glue.CircuitBreaker
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception  (Exception, throwIO, try)

data CircuitBreakerTestException = CircuitBreakerTestException
  deriving (Eq, Show)

instance Exception CircuitBreakerTestException

requests :: [Int]
requests = [1 .. 10]

spec :: Spec
spec = do
  describe "circuitBreaker" $ do
    it "Multiple failures prevent subsequent calls" $
      property $ \failureMax -> do
        let positiveFailureMax     = abs failureMax `mod` 5
            options                = defaultCircuitBreakerOptions { maxBreakerFailures = positiveFailureMax }
        ref                        <- newIORef (0 :: Int)
        let service _              = atomicModifyIORef' ref (\c -> (c + 1, ())) >> throwIO CircuitBreakerTestException :: IO Int
        (s, circuitBreakerService) <- circuitBreaker options service
        results                    <- traverse (try . try . circuitBreakerService) requests :: IO [Either CircuitBreakerException (Either CircuitBreakerTestException Int)]
        let expectedResults        = replicate (positiveFailureMax + 1) (Right $ Left CircuitBreakerTestException)
                                  ++ replicate (10 - positiveFailureMax - 1) (Left $ CircuitBreakerException "Circuit breaker open.")
        results `shouldBe` expectedResults
        isCircuitBreakerOpen s `shouldReturn` True
        isCircuitBreakerClosed s `shouldReturn` False
        readIORef ref `shouldReturn` (positiveFailureMax + 1)

    it "Successful calls pass straight through" $
      property $ \(failureMax :: Int, requestIDs :: [Int]) -> do
        let positiveFailureMax     = abs failureMax
            options                = defaultCircuitBreakerOptions { maxBreakerFailures = positiveFailureMax }
            service req            = pure $ req + 1
        (s, circuitBreakerService) <- circuitBreaker options service
        results                    <- traverse circuitBreakerService requestIDs
        let expectedResults        = fmap (+ 1) requestIDs
        results `shouldBe` expectedResults
        isCircuitBreakerOpen s `shouldReturn` False
        isCircuitBreakerClosed s `shouldReturn` True

    it "combineCircuitBreakerStates reports open if any breaker is open" $ do
      let opts = defaultCircuitBreakerOptions { maxBreakerFailures = 0 }
      (s1, svc1) <- circuitBreaker opts (\(_ :: Int) -> throwIO CircuitBreakerTestException :: IO Int)
      (s2, _)    <- circuitBreaker opts (\(x :: Int) -> pure (x + 1) :: IO Int)
      let combined = combineCircuitBreakerStates [s1, s2]
      isCircuitBreakerOpen combined `shouldReturn` False
      isCircuitBreakerClosed combined `shouldReturn` True
      _ <- try (svc1 1) :: IO (Either CircuitBreakerTestException Int)
      isCircuitBreakerOpen combined `shouldReturn` True
      isCircuitBreakerClosed combined `shouldReturn` False
