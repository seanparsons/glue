{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.EkgSpec where

import           Control.Exception.Base      hiding (throw, throwIO)
import           Control.Exception.Lifted
import qualified Data.HashMap.Strict         as M
import           Data.Int
import           Data.Text
import           Data.Typeable
import           Glue.Ekg
import           Glue.Types
import           System.Metrics
import qualified System.Metrics.Distribution as MD
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances   ()

data EkgTestException = EkgTestException deriving (Eq, Show, Typeable)
instance Exception EkgTestException

data MetricsResult = CounterResult Int64
                   | GaugeResult Int64
                   | LabelResult Text
                   | DistributionResult Int64
                    deriving (Eq, Show)

checkResult :: Store -> Text -> (MetricsResult -> Expectation) -> Expectation
checkResult store name check = do
  allMetrics <- sampleAll store
  let possibleValue = M.lookup name allMetrics
  result <- case possibleValue of
                (Just (Counter counterCount)) -> return $ CounterResult counterCount
                (Just (Gauge value))          -> return $ GaugeResult value
                (Just (Label text))           -> return $ LabelResult text
                (Just (Distribution stats))   -> return $ DistributionResult $ MD.count stats
                Nothing                       -> fail "No metric."
  check result

testStats :: String ->
            (Store -> Text -> BasicService IO Int Int -> IO (BasicService IO Int Int)) ->
            (Int -> Int -> MetricsResult -> Expectation) ->
            (Int -> Int -> MetricsResult -> Expectation) ->
            Spec
testStats methodName method successCheck failureCheck =
  describe methodName $ do
    it "Successful call" $ do
      property $ \(request :: Int, result :: Int, name :: Text) -> do
        let service _ = return result :: IO Int
        store <- newStore
        wrappedService <- method store name service :: IO (BasicService IO Int Int)
        (wrappedService request) `shouldReturn` result
        checkResult store name $ successCheck request result
    it "Failing call" $ do
      property $ \(request :: Int, result :: Int, name :: Text) -> do
        let service _ = throwIO EkgTestException :: IO Int
        store <- newStore
        wrappedService <- method store name service :: IO (BasicService IO Int Int)
        (wrappedService request) `shouldThrow` (== EkgTestException)
        checkResult store name $ failureCheck request result

spec :: Spec
spec = parallel $ do
  testStats "recordDistribution" recordDistribution (\_ -> \_ -> \m -> m `shouldBe` (DistributionResult 1)) (\_ -> \_ -> \m -> m `shouldBe` (DistributionResult 1))
  testStats "recordAttempts" recordAttempts (\_ -> \_ -> \m -> m `shouldBe` (CounterResult 1)) (\_ -> \_ -> \m -> m `shouldBe` (CounterResult 1))
  testStats "recordSuccesses" recordSuccesses (\_ -> \_ -> \m -> m `shouldBe` (CounterResult 1)) (\_ -> \_ -> \m -> m `shouldBe` (CounterResult 0))
  testStats "recordFailures" recordFailures (\_ -> \_ -> \m -> m `shouldBe` (CounterResult 0)) (\_ -> \_ -> \m -> m `shouldBe` (CounterResult 1))
  testStats "recordLastRequest" recordLastRequest (\r -> \_ -> \m -> m `shouldBe` (LabelResult $ pack $ show r)) (\r -> \_ -> \m -> m `shouldBe` (LabelResult $ pack $ show r))
  testStats "recordLastResult" recordLastResult (\_ -> \r -> \m -> m `shouldBe` (LabelResult $ pack $ show r)) (\_ -> \_ -> \m -> m `shouldBe` (LabelResult ""))
