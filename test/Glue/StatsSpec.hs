{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Glue.StatsSpec where

import Data.Typeable
import Glue.Stats
import Glue.Types
import Test.QuickCheck.Instances()
import Test.Hspec
import Data.Text
import Test.QuickCheck
import Control.Exception.Base hiding (throw, throwIO)
import Control.Exception.Lifted
import System.Metrics

{-
recordDistribution
  , recordAttempts
  , recordSuccesses
  , recordFailures
  , recordLastRequest
  , recordLastResult
-}

data StatsTestException = StatsTestException deriving (Eq, Show, Typeable)
instance Exception StatsTestException

testStats :: String -> 
            (Store -> Text -> BasicService IO Int Int -> IO (BasicService IO Int Int)) ->
            (Store -> Expectation) ->
            (Store -> Expectation) ->
            Spec
testStats methodName method successCheck failureCheck = 
  describe methodName $ do
    it "Successful call" $ do
      property $ \(request :: Int, result :: Int, name :: Text) -> do
        let service _ = return result :: IO Int
        store <- newStore
        wrappedService <- method store name service :: IO (BasicService IO Int Int)
        (wrappedService request) `shouldReturn` result
        successCheck store
    it "Failing call" $ do
      property $ \(request :: Int, name :: Text) -> do
        let service _ = throwIO StatsTestException :: IO Int
        store <- newStore
        wrappedService <- method store name service :: IO (BasicService IO Int Int)
        (wrappedService request) `shouldThrow` (== StatsTestException)
        failureCheck store

spec :: Spec
spec = do
  testStats "recordDistribution" recordDistribution (\_ -> pending) (\_ -> pending)
  testStats "recordAttempts" recordAttempts (\_ -> pending) (\_ -> pending)
  testStats "recordSuccesses" recordSuccesses (\_ -> pending) (\_ -> pending)
  testStats "recordFailures" recordFailures (\_ -> pending) (\_ -> pending)
  testStats "recordLastRequest" recordLastRequest (\_ -> pending) (\_ -> pending)
  testStats "recordLastResult" recordLastResult (\_ -> pending) (\_ -> pending)