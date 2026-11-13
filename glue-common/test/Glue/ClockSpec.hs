{-# LANGUAGE ScopedTypeVariables #-}

module Glue.ClockSpec (spec) where

import           Control.Concurrent.Async (async, poll, wait)
import           Data.IORef
import           Glue.Clock
import           Glue.Clock.Virtual
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "systemClock" $ do
    it "Never goes backwards and sleeps at least as long as asked" $ do
      let clock = systemClock :: Clock IO
      start <- clockNow clock
      clockSleep clock 2000
      end <- clockNow clock
      (end - start) `shouldSatisfy` (>= 2000)

    it "Times out actions that run past the limit" $ do
      let clock = systemClock :: Clock IO
      clockTimeout clock 1000 (clockSleep clock 1000000) `shouldReturn` Nothing
      clockTimeout clock 1000000 (pure (5 :: Int)) `shouldReturn` Just 5

  describe "virtualClock" $ do
    it "Only moves when advanced" $
      property $ \(steps :: [NonNegative Int]) -> do
        vc <- newVirtualClock
        mapM_ (advanceClock vc . getNonNegative) steps
        virtualNow vc `shouldReturn` fromIntegral (sum (fmap getNonNegative steps))

    it "Wakes sleepers in deadline order" $ do
      vc <- newVirtualClock
      let clock = virtualClock vc :: Clock IO
      order <- newIORef []
      let sleeper (n :: Int) = clockSleep clock (n * 1000) >> atomicModifyIORef' order (\o -> (o ++ [n], ()))
      threads <- traverse (async . sleeper) [3, 1, 2]
      waitForSleepers vc 3
      advanceClock vc 2000
      readIORef order `shouldReturn` [1, 2]
      sleeperCount vc `shouldReturn` 1
      advanceClock vc 1000
      mapM_ wait threads
      readIORef order `shouldReturn` [1, 2, 3]

    it "Does not wake a sleeper before its deadline" $ do
      vc <- newVirtualClock
      let clock = virtualClock vc :: Clock IO
      sleeping <- async (clockSleep clock 1000)
      waitForSleepers vc 1
      advanceClock vc 999
      poll sleeping >>= (`shouldSatisfy` maybe True (const False))
      advanceClock vc 1
      wait sleeping

    it "Times out exactly at the limit" $ do
      vc <- newVirtualClock
      let clock = virtualClock vc :: Clock IO
      result <- async (clockTimeout clock 5000 (clockSleep clock 10000 >> pure ()))
      waitForSleepers vc 2
      advanceClock vc 5000
      wait result `shouldReturn` Nothing
      sleeperCount vc `shouldReturn` 0

    it "remainingUntil reports milliseconds left, never negative" $ do
      vc <- newVirtualClock
      let clock = virtualClock vc :: Clock IO
      remainingUntil clock 5000 `shouldReturn` Just 5
      advanceClock vc 7000
      remainingUntil clock 5000 `shouldReturn` Just 0
