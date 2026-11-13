{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.PrometheusSpec (spec) where

import           Data.IORef
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.Map.Strict       as M
import           Data.Text             (Text)
import           Glue.CircuitBreaker
import           Glue.Clock.Virtual
import           Glue.ConcurrencyLimit
import           Glue.Hedging
import           Glue.Prometheus
import           Glue.Retry
import           Glue.Shutdown
import           Prometheus            (Metric (..), Sample (..),
                                        SampleGroup (..), getCounter, getGauge,
                                        getHistogram, getVectorWith)
import           Test.Hspec
import           UnliftIO.Async        (async, wait)
import           UnliftIO.Exception    (Exception, SomeException, throwIO, try)
import           UnliftIO.STM          (atomically, newEmptyTMVarIO, putTMVar,
                                        readTMVar)

data PrometheusTestException = PrometheusTestException
  deriving (Eq, Show)

instance Exception PrometheusTestException

counters :: Ord l => (m -> IO Double) -> IO [(l, m)] -> IO (M.Map l Double)
counters reader entries = entries >>= fmap M.fromList . traverse (\(l, m) -> (,) l <$> reader m)

observations :: [(Text, M.Map Double Int)] -> M.Map Text Int
observations = M.fromList . fmap (fmap sum)

spec :: Spec
spec = do
  describe "instrumentService" $ do
    it "Records calls, failures and durations per service" $ do
      metrics <- registerServiceMetrics
      let ok      = instrumentService metrics "ok" (\(x :: Int) -> pure x)
          failing = instrumentService metrics "failing" (\(_ :: Int) -> throwIO PrometheusTestException :: IO Int)
      _ <- traverse ok [1, 2, 3]
      failing 1 `shouldThrow` (== PrometheusTestException)
      getVectorWith (serviceCalls metrics) getCounter >>= (`shouldBe` M.fromList [("failing", 1), ("ok", 3)]) . M.fromList
      getVectorWith (serviceFailures metrics) getCounter >>= (`shouldBe` M.fromList [("failing", 1)]) . M.fromList
      observations <$> getVectorWith (serviceDuration metrics) getHistogram
        `shouldReturn` M.fromList [("failing", 1), ("ok", 3)]

    it "Counts Left results as failures for Either services" $ do
      metrics <- registerServiceMetrics
      let service = instrumentServiceEither metrics "either" (\(x :: Int) -> pure (if x > 0 then Right x else Left ("bad" :: String)))
      _ <- traverse service [1, 0, -1]
      M.fromList <$> getVectorWith (serviceFailures metrics) getCounter `shouldReturn` M.fromList [("either", 2)]

  describe "instrumentBreakerOptions" $
    it "Tracks the breaker phase and counts transitions, keeping the existing observer" $ do
      metrics <- registerBreakerMetrics
      vc      <- newVirtualClock
      seen    <- newIORef (0 :: Int)
      options <- instrumentBreakerOptions metrics "backend"
        (defaultCircuitBreakerOptions :: CircuitBreakerOptions IO SomeException)
          { breakerFailureThreshold = 1
          , breakerClock            = virtualClock vc
          , onBreakerTransition     = const (atomicModifyIORef' seen (\n -> (n + 1, ())))
          }
      counters getGauge (getVectorWith (breakerState metrics) pure) `shouldReturn` M.fromList [("backend", 0)]
      (_, breaker) <- circuitBreaker options (\(_ :: Int) -> throwIO PrometheusTestException :: IO Int)
      _ <- try (breaker 1) :: IO (Either PrometheusTestException Int)
      counters getGauge (getVectorWith (breakerState metrics) pure) `shouldReturn` M.fromList [("backend", 2)]
      counters getCounter (getVectorWith (breakerTransitions metrics) pure)
        `shouldReturn` M.fromList [(("backend", "closed", "open"), 1)]
      readIORef seen `shouldReturn` 1

  describe "instrumentRetryOptions" $
    it "Counts retries and their waits" $ do
      metrics <- registerRetryMetrics
      calls   <- newIORef (0 :: Int)
      let options = instrumentRetryOptions metrics "flaky" (defaultRetryOptions :: RetryOptions IO Int SomeException) { maximumRetries = 2 }
          service (x :: Int) = atomicModifyIORef' calls (\n -> (n + 1, n + 1)) >>= \n -> if n < 3 then throwIO PrometheusTestException else pure x
      retryingService options service 5 `shouldReturn` 5
      counters getCounter (getVectorWith (retryCount metrics) pure) `shouldReturn` M.fromList [("flaky", 2)]
      observations <$> getVectorWith (retryDelay metrics) getHistogram `shouldReturn` M.fromList [("flaky", 2)]

  describe "instrumentAdmissionStore" $
    it "Tracks slots in flight and refusals per limiter" $ do
      metrics <- registerConcurrencyMetrics
      store   <- instrumentAdmissionStore metrics "providers" <$> newInMemoryAdmissionStore (const 1)
      gate    <- newEmptyTMVarIO
      started <- newEmptyTMVarIO
      let held = withSlot store ("a" :: Text) (atomically (putTMVar started ()) >> atomically (readTMVar gate))
      waiting <- async held
      atomically $ readTMVar started
      counters getGauge (getVectorWith (concurrencyInFlight metrics) pure) `shouldReturn` M.fromList [("providers", 1)]
      withSlot store "a" (pure ()) `shouldReturn` Left Saturated
      counters getCounter (getVectorWith (concurrencyRefusals metrics) pure) `shouldReturn` M.fromList [("providers", 1)]
      atomically $ putTMVar gate ()
      wait waiting `shouldReturn` Right ()
      counters getGauge (getVectorWith (concurrencyInFlight metrics) pure) `shouldReturn` M.fromList [("providers", 0)]

  describe "instrumentHedgeOptions" $
    it "Counts backup launches, attempt outcomes and the winner" $ do
      metrics <- registerHedgeMetrics
      let options = instrumentHedgeOptions metrics "search" (defaultHedgeOptions :: HedgeOptions IO SomeException Int)
      hedgeAcross options ((\_ -> throwIO PrometheusTestException) :| [pure]) (7 :: Int) `shouldReturn` 7
      counters getCounter (getVectorWith (hedgeLaunches metrics) pure) `shouldReturn` M.fromList [(("search", "1"), 1)]
      counters getCounter (getVectorWith (hedgeAttempts metrics) pure)
        `shouldReturn` M.fromList [(("search", "0", "failed"), 1), (("search", "1", "answered"), 1)]
      counters getCounter (getVectorWith (hedgeWins metrics) pure) `shouldReturn` M.fromList [(("search", "1"), 1)]

  describe "shutdownInFlightMetric" $
    it "Samples each handle's in-flight count at collection" $ do
      gate    <- newEmptyTMVarIO
      started <- newEmptyTMVarIO
      (service, handle) <- gracefulShutdown (defaultShutdownOptions :: ShutdownOptions IO Int) $ \(x :: Int) -> do
        atomically $ putTMVar started ()
        atomically $ readTMVar gate
        pure x
      ((), collect) <- construct (shutdownInFlightMetric [("api", handle)])
      let values = fmap (\groups -> [(labels, value) | SampleGroup _ _ samples <- groups, Sample _ labels value <- samples]) collect
      values `shouldReturn` [([("service", "api")], "0.0")]
      call <- async (service 1)
      atomically $ readTMVar started
      values `shouldReturn` [([("service", "api")], "1.0")]
      atomically $ putTMVar gate ()
      wait call `shouldReturn` 1
