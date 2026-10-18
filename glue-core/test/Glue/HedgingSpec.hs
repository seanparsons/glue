{-# LANGUAGE ScopedTypeVariables #-}

module Glue.HedgingSpec (spec) where

import           Control.Concurrent (threadDelay)
import           Data.IORef
import           Glue.Hedging
import           Test.Hspec
import           UnliftIO.Exception (Exception, throwIO)

data HedgingTestException = HedgingTestException
  deriving (Eq, Show)

instance Exception HedgingTestException

spec :: Spec
spec = do
  describe "hedgingService" $ do
    it "Completes with the fast hedged call when the primary call is slow" $ do
      ref <- newIORef (0 :: Int)
      let service (req :: Int) = do
            attemptNum <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
            if attemptNum == 1
              then threadDelay (500 * 1000) >> pure (req + 100)
              else pure (req * 2)
          hedged = hedgingService (HedgingOptions { hedgingCount = 2, hedgingDelayMs = 5 }) service
      hedged 21 `shouldReturn` 42

    it "Succeeds if one attempt fails immediately and another succeeds" $ do
      ref <- newIORef (0 :: Int)
      let service (req :: Int) = do
            attemptNum <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
            if attemptNum == 1
              then throwIO HedgingTestException
              else pure (req * 3)
          hedged = hedgingService (HedgingOptions { hedgingCount = 2, hedgingDelayMs = 0 }) service
      hedged 10 `shouldReturn` 30

    it "Rethrows the exception if all hedged attempts fail" $ do
      let service (_ :: Int) = throwIO HedgingTestException :: IO Int
          hedged = hedgingService (HedgingOptions { hedgingCount = 3, hedgingDelayMs = 0 }) service
      hedged 1 `shouldThrow` (== HedgingTestException)
