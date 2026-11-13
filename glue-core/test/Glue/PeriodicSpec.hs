{-# LANGUAGE ScopedTypeVariables #-}

module Glue.PeriodicSpec (spec) where

import           Control.Concurrent       (yield)
import           Control.Concurrent.Async (async, wait)
import           Data.IORef
import           Glue.Clock
import           Glue.Clock.Virtual
import           Glue.Periodic
import           Test.Hspec
import           UnliftIO.Exception       (Exception, SomeException,
                                           fromException, throwIO)
import           UnliftIO.STM             (atomically, newEmptyTMVarIO,
                                           putTMVar, readTMVar)

data PassFailed = PassFailed
  deriving (Eq, Show)

instance Exception PassFailed

waitUntil :: IO Bool -> IO ()
waitUntil condition = condition >>= \done -> if done then pure () else yield >> waitUntil condition

options :: VirtualClock -> PeriodicOptions IO
options vc = defaultPeriodicOptions { periodMs = 100, periodicGracePeriodMs = 50, periodicClock = virtualClock vc }

spec :: Spec
spec = do
  describe "periodically" $ do
    it "Runs a pass every period, measured from the end of the previous pass" $ do
      vc     <- newVirtualClock
      starts <- newIORef []
      let pass = do
            now <- virtualNow vc
            atomicModifyIORef' starts (\s -> (s ++ [now], ()))
            clockSleep (virtualClock vc) 30000
      handle <- periodically (options vc) pass
      waitForSleepers vc 1
      advanceClock vc 100000
      waitForSleepers vc 1
      advanceClock vc 30000
      waitUntil ((== 1) <$> passesCompleted handle)
      waitForSleepers vc 1
      advanceClock vc 100000
      waitForSleepers vc 1
      advanceClock vc 30000
      waitUntil ((== 2) <$> passesCompleted handle)
      readIORef starts `shouldReturn` [100000, 230000]
      stopPeriodic handle `shouldReturn` True

    it "Runs the first pass immediately when asked" $ do
      vc <- newVirtualClock
      handle <- periodically (options vc) { runImmediately = True } (pure ())
      waitUntil ((== 1) <$> passesCompleted handle)
      virtualNow vc `shouldReturn` 0
      stopPeriodic handle `shouldReturn` True

    it "Adds jitter from the random source to the period" $ do
      vc <- newVirtualClock
      handle <- periodically (options vc) { periodJitterMs = 40, periodicRandom = \n -> pure (n `div` 2) } (pure ())
      waitForSleepers vc 1
      advanceClock vc 119000
      passesCompleted handle `shouldReturn` 0
      advanceClock vc 1000
      waitUntil ((== 1) <$> passesCompleted handle)
      stopPeriodic handle `shouldReturn` True

    it "Reports a failed pass and keeps running" $ do
      vc       <- newVirtualClock
      failures <- newIORef (0 :: Int)
      calls    <- newIORef (0 :: Int)
      let pass = do
            n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
            if n == 1 then throwIO PassFailed else pure ()
          onFailed (e :: SomeException) = case fromException e of
            Just PassFailed -> atomicModifyIORef' failures (\f -> (f + 1, ()))
            Nothing         -> pure ()
      handle <- periodically (options vc) { onPassFailed = onFailed } pass
      runNow handle `shouldReturn` True
      runNow handle `shouldReturn` True
      readIORef failures `shouldReturn` 1
      readIORef calls `shouldReturn` 2
      passesCompleted handle `shouldReturn` 2
      stopPeriodic handle `shouldReturn` True

    it "runNow queues behind a running pass" $ do
      vc    <- newVirtualClock
      gate  <- newEmptyTMVarIO
      calls <- newIORef (0 :: Int)
      let pass = do
            n <- atomicModifyIORef' calls (\c -> (c + 1, c + 1))
            if n == 1 then atomically (readTMVar gate) else pure ()
      handle <- periodically (options vc) { runImmediately = True } pass
      waitUntil ((== 1) <$> readIORef calls)
      queued <- async (runNow handle)
      atomically $ putTMVar gate ()
      wait queued `shouldReturn` True
      readIORef calls `shouldReturn` 2
      stopPeriodic handle `shouldReturn` True

    it "Stopping lets a running pass finish within the grace period" $ do
      vc       <- newVirtualClock
      gate     <- newEmptyTMVarIO
      started  <- newEmptyTMVarIO
      finished <- newIORef False
      let pass = do
            atomically $ putTMVar started ()
            atomically $ readTMVar gate
            writeIORef finished True
      handle <- periodically (options vc) { runImmediately = True } pass
      atomically $ readTMVar started
      stopping <- async (stopPeriodic handle)
      waitForSleepers vc 1
      atomically $ putTMVar gate ()
      wait stopping `shouldReturn` True
      readIORef finished `shouldReturn` True

    it "Stopping cancels a pass that outlasts the grace period" $ do
      vc        <- newVirtualClock
      started   <- newEmptyTMVarIO
      finished  <- newIORef False
      let pass = do
            atomically $ putTMVar started ()
            clockSleep (virtualClock vc) 1000000
            writeIORef finished True
      handle <- periodically (options vc) { runImmediately = True } pass
      atomically $ readTMVar started
      stopping <- async (stopPeriodic handle)
      waitForSleepers vc 2
      advanceClock vc 50000
      wait stopping `shouldReturn` False
      readIORef finished `shouldReturn` False
      runNow handle `shouldReturn` False
