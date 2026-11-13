{-# LANGUAGE ScopedTypeVariables #-}

module Glue.HedgingSpec (spec) where

import           Control.Concurrent       (yield)
import           Control.Concurrent.Async (async, wait)
import           Data.IORef
import           Data.List.NonEmpty       (NonEmpty (..))
import           Glue.Clock
import           Glue.Clock.Virtual
import           Glue.Failure
import           Glue.Hedging
import           Glue.Race
import           Test.Hspec
import           UnliftIO.Exception       (Exception, SomeException, finally,
                                           fromException, throwIO)
import           UnliftIO.STM             (atomically, newEmptyTMVarIO,
                                           putTMVar, readTMVar)

data HedgingTestException = HedgingTestException Int
  deriving (Eq, Show)

instance Exception HedgingTestException

-- | How an attempt ended, without the payload, for comparing reports.
data Ended = Answered Int | Failed Int | Cancelled | Crashed
  deriving (Eq, Show)

summarise :: AttemptOutcome SomeException Int -> Ended
summarise (AttemptAnswered n)  = Answered n
summarise (AttemptFailed e)    = maybe Crashed (\(HedgingTestException n) -> Failed n) (fromException e)
summarise AttemptCancelled     = Cancelled
summarise (AttemptCrashed _)   = Crashed

-- | Options on a virtual clock that record launches, attempt endings and the winner.
recording :: VirtualClock -> (Int -> IO Bool) -> IO (HedgeOptions IO SomeException Int, IORef [Int], IORef [(Int, Ended)], IORef [Int])
recording vc admit = do
  launches <- newIORef []
  endings  <- newIORef []
  winners  <- newIORef []
  let record ref x = atomicModifyIORef' ref (\xs -> (xs ++ [x], ()))
      options = (defaultHedgeOptions :: HedgeOptions IO SomeException Int)
        { hedgeDelayMs   = 100
        , admitLaunch    = \i -> record launches i >> admit i
        , onAttemptEnded = \i o -> record endings (i, summarise o)
        , onHedgeWon     = record winners
        , hedgeClock     = virtualClock vc
        }
  pure (options, launches, endings, winners)

spec :: Spec
spec = do
  describe "hedgeAcross" $ do
    it "Returns the primary's answer without launching a backup inside the delay" $ do
      vc <- newVirtualClock
      gate <- newEmptyTMVarIO
      (options, launches, endings, winners) <- recording vc (const (pure True))
      call <- async $ hedgeAcross options ((\x -> atomically (readTMVar gate) >> pure x) :| [pure . negate]) 5
      waitForSleepers vc 1
      atomically $ putTMVar gate ()
      wait call `shouldReturn` 5
      readIORef launches `shouldReturn` []
      readIORef endings `shouldReturn` [(0, Answered 5)]
      readIORef winners `shouldReturn` [0]

    it "Launches the backup after the delay and cancels the slow primary" $ do
      vc <- newVirtualClock
      closed <- newIORef False
      (options, launches, endings, winners) <- recording vc (const (pure True))
      let primary x = (clockSleep (virtualClock vc) 500000 >> pure x) `finally` writeIORef closed True
      call <- async $ hedgeAcross options (primary :| [pure . negate]) 5
      waitForSleepers vc 2
      advanceClock vc 100000
      wait call `shouldReturn` (-5)
      readIORef closed `shouldReturn` True
      readIORef launches `shouldReturn` [1]
      readIORef endings `shouldReturn` [(1, Answered (-5)), (0, Cancelled)]
      readIORef winners `shouldReturn` [1]

    it "Keeps waiting on the primary when the backup's launch is declined" $ do
      vc <- newVirtualClock
      (options, launches, endings, _) <- recording vc (const (pure False))
      let primary x = clockSleep (virtualClock vc) 500000 >> pure x
      call <- async $ hedgeAcross options (primary :| [pure . negate]) 5
      waitForSleepers vc 2
      advanceClock vc 100000
      let waitForDecision = readIORef launches >>= \l -> if null l then yield >> waitForDecision else pure ()
      waitForDecision
      advanceClock vc 400000
      wait call `shouldReturn` 5
      readIORef launches `shouldReturn` [1]
      readIORef endings `shouldReturn` [(0, Answered 5)]

    it "Launches the backup immediately when the primary fails early" $ do
      vc <- newVirtualClock
      (options, launches, endings, _) <- recording vc (const (pure True))
      hedgeAcross options ((\_ -> throwIO (HedgingTestException 0)) :| [pure . negate]) 5 `shouldReturn` (-5)
      readIORef launches `shouldReturn` [1]
      readIORef endings `shouldReturn` [(0, Failed 0), (1, Answered (-5))]

    it "Rethrows the first failure when every attempt fails" $ do
      vc <- newVirtualClock
      (options, _, endings, winners) <- recording vc (const (pure True))
      let failing n _ = throwIO (HedgingTestException n) :: IO Int
      hedgeAcross options (failing 0 :| [failing 1, failing 2]) (5 :: Int) `shouldThrow` (== HedgingTestException 0)
      readIORef endings `shouldReturn` [(0, Failed 0), (1, Failed 1), (2, Failed 2)]
      readIORef winners `shouldReturn` []

    it "Rethrows the primary's failure when the backup is declined" $ do
      vc <- newVirtualClock
      (options, _, _, _) <- recording vc (const (pure False))
      hedgeAcross options ((\_ -> throwIO (HedgingTestException 0)) :| [pure . negate]) 5
        `shouldThrow` (== HedgingTestException 0)

  describe "hedgeAcrossEither" $
    it "Treats Left as a failed attempt and returns the first Left if all fail" $ do
      vc <- newVirtualClock
      let options = (defaultHedgeOptions :: HedgeOptions IO String (Either String Int)) { hedgeClock = virtualClock vc }
      hedgeAcrossEither options ((\_ -> pure (Left "primary")) :| [pure . Right]) 3 `shouldReturn` Right 3
      hedgeAcrossEither options ((\_ -> pure (Left "primary")) :| [\_ -> pure (Left "backup")]) (3 :: Int)
        `shouldReturn` (Left "primary" :: Either String Int)

  describe "Glue.Race" $ do
    it "Reports attempts that ended together in index order" $ do
      result <- withRace exceptionFailures systemClock $ \race -> do
        _ <- launch race (pure (1 :: Int))
        _ <- launch race (pure 2)
        let settle = finishedAttempts race >>= \f -> if length f == 2 then pure () else yield >> settle
        settle
        first  <- awaitNext race Nothing
        second <- awaitNext race Nothing
        none   <- awaitNext race Nothing
        pure (fmap eventText [first, second, none])
      result `shouldBe` ["0 answered 1", "1 answered 2", "nothing running"]

    it "Abandoning an attempt that already completed reports its result" $ do
      result <- withRace exceptionFailures systemClock $ \race -> do
        index <- launch race (pure (7 :: Int))
        let settle = finishedAttempts race >>= \f -> if f == [index] then pure () else yield >> settle
        settle
        summarise <$> abandon race index
      result `shouldBe` Answered 7

    it "Times out when no attempt ends within the limit" $ do
      vc <- newVirtualClock
      gate <- newEmptyTMVarIO
      let clock = virtualClock vc :: Clock IO
      waiting <- async $ withRace exceptionFailures clock $ \race -> do
        _ <- launch race (atomically (readTMVar gate) >> pure (1 :: Int))
        eventText <$> awaitNext race (Just 50)
      waitForSleepers vc 1
      advanceClock vc 50000
      wait waiting `shouldReturn` "timed out"
      atomically $ putTMVar gate ()

  describe "hedgingService" $ do
    it "Completes with the fast hedged call when the primary call is slow" $ do
      vc  <- newVirtualClock
      ref <- newIORef (0 :: Int)
      let clock = virtualClock vc :: Clock IO
          service (req :: Int) = do
            attemptNum <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
            if attemptNum == 1
              then clockSleep clock (500 * 1000) >> pure (req + 100)
              else pure (req * 2)
          hedged = hedgingService ((defaultHedgingOptions :: HedgingOptions IO) { hedgingCount = 2, hedgingDelayMs = 5, hedgingClock = clock }) service
      call <- async (hedged 21)
      waitForSleepers vc 2
      advanceClock vc 5000
      wait call `shouldReturn` 42

    it "Succeeds if one attempt fails immediately and another succeeds" $ do
      ref <- newIORef (0 :: Int)
      let service (req :: Int) = do
            attemptNum <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
            if attemptNum == 1
              then throwIO (HedgingTestException 1)
              else pure (req * 3)
          hedged = hedgingService (defaultHedgingOptions { hedgingCount = 2, hedgingDelayMs = 0 }) service
      hedged 10 `shouldReturn` 30

    it "Rethrows the exception if all hedged attempts fail" $ do
      let service (_ :: Int) = throwIO (HedgingTestException 1) :: IO Int
          hedged = hedgingService (defaultHedgingOptions { hedgingCount = 3, hedgingDelayMs = 0 }) service
      hedged 1 `shouldThrow` (== HedgingTestException 1)
  where
    eventText (AttemptEnded i (AttemptAnswered n)) = show i ++ " answered " ++ show (n :: Int)
    eventText (AttemptEnded i _)                   = show i ++ " ended"
    eventText RaceTimedOut                         = "timed out"
    eventText NothingRunning                       = "nothing running"
