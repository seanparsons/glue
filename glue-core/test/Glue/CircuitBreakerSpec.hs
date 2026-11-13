{-# LANGUAGE ScopedTypeVariables #-}

module Glue.CircuitBreakerSpec (spec) where

import           Control.Concurrent.Async (async, cancel, wait)
import           Control.Monad            (forM, replicateM_)
import           Data.IORef
import           Glue.CircuitBreaker
import           Glue.Clock
import           Glue.Clock.Virtual
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception       (Exception, fromException, throwIO,
                                           try)
import           UnliftIO.STM             (atomically, newEmptyTMVarIO,
                                           putTMVar, readTMVar)

data CircuitBreakerTestException = CircuitBreakerTestException
  deriving (Eq, Show)

instance Exception CircuitBreakerTestException

newtype BadRequestException = BadRequestException Int
  deriving (Eq, Show)

instance Exception BadRequestException

type Outcome = Either CircuitBreakerException (Either CircuitBreakerTestException Int)

failing :: IORef Int -> Int -> IO Int
failing ref _ = atomicModifyIORef' ref (\c -> (c + 1, ())) >> throwIO CircuitBreakerTestException

callAll :: (Int -> IO Int) -> [Int] -> IO [Outcome]
callAll service = traverse (try . try . service)

options :: VirtualClock -> CircuitBreakerOptions IO failure
options vc = defaultCircuitBreakerOptions { breakerClock = virtualClock vc }

openFailure :: Outcome
openFailure = Left (CircuitBreakerException "Circuit breaker open.")

-- | A scripted outcome for comparing the exception and value shapes.
data Step = Succeed Int | Fail Bool | Wait Int
  deriving (Show)

stepValue :: Step -> Int
stepValue (Succeed n) = n
stepValue (Wait n)    = n
stepValue (Fail _)    = 0

-- | A failure carrying whether it should count against the breaker.
newtype StepFailure = StepFailure Bool
  deriving (Eq, Show)

instance Exception StepFailure

instance Arbitrary Step where
  arbitrary = oneof [Succeed <$> arbitrary, Fail <$> arbitrary, Wait <$> choose (0, 30)]

spec :: Spec
spec = do
  describe "circuitBreaker" $ do
    it "Opens on the failure number given by the threshold" $
      property $ \(threshold' :: Int) -> do
        let threshold = 1 + abs threshold' `mod` 5
        vc  <- newVirtualClock
        ref <- newIORef 0
        (s, service) <- circuitBreaker ((options vc) { breakerFailureThreshold = threshold }) (failing ref)
        results <- callAll service [1 .. 10]
        results `shouldBe` replicate threshold (Right (Left CircuitBreakerTestException)) ++ replicate (10 - threshold) openFailure
        isCircuitBreakerOpen s `shouldReturn` True
        isCircuitBreakerClosed s `shouldReturn` False
        readIORef ref `shouldReturn` threshold

    it "Successful calls pass straight through" $
      property $ \(requestIDs :: [Int]) -> do
        (s, service) <- circuitBreaker defaultCircuitBreakerOptions (\req -> pure (req + 1) :: IO Int)
        traverse service requestIDs `shouldReturn` fmap (+ 1) requestIDs
        isCircuitBreakerClosed s `shouldReturn` True

    it "Counts consecutive failures, a success resetting the count" $ do
      vc <- newVirtualClock
      (s, service) <- circuitBreaker (options vc) $ \(req :: Int) ->
        if req > 0 then pure req else throwIO CircuitBreakerTestException
      _ <- callAll service [0, 0, 1, 0, 0, 1, 0, 0]
      isCircuitBreakerClosed s `shouldReturn` True
      _ <- callAll service [0]
      isCircuitBreakerOpen s `shouldReturn` True

    it "With a failure window, only failures within the window count" $ do
      vc  <- newVirtualClock
      ref <- newIORef 0
      (s, service) <- circuitBreaker ((options vc) { breakerFailureWindowMs = Just 1000 }) (failing ref)
      replicateM_ 5 $ do
        _ <- callAll service [1, 1]
        advanceClock vc 1000000
      isCircuitBreakerClosed s `shouldReturn` True
      _ <- callAll service [1, 1]
      advanceClock vc 999000
      _ <- callAll service [1]
      isCircuitBreakerOpen s `shouldReturn` True

    it "A call cancelled by an asynchronous exception does not count as a failure" $ do
      vc      <- newVirtualClock
      started <- newEmptyTMVarIO
      (s, service) <- circuitBreaker ((options vc) { breakerFailureThreshold = 1 }) $ \(req :: Int) -> do
        atomically $ putTMVar started ()
        clockSleep (virtualClock vc) 1000000
        pure req
      call <- async (service 1)
      atomically $ readTMVar started
      cancel call
      isCircuitBreakerClosed s `shouldReturn` True

    it "Stays open for the open duration, then admits a single trial" $ do
      vc   <- newVirtualClock
      gate <- newEmptyTMVarIO
      healthy <- newIORef False
      let service (req :: Int) = do
            ok <- readIORef healthy
            if ok then atomically (readTMVar gate) >> pure req else throwIO CircuitBreakerTestException
      (s, breaker) <- circuitBreaker ((options vc) { breakerFailureThreshold = 1, breakerOpenDurationMs = 500 }) service
      _ <- callAll breaker [1]
      advanceClock vc 499000
      callAll breaker [1] `shouldReturn` [openFailure]
      advanceClock vc 1000
      writeIORef healthy True
      trial <- async (breaker 7)
      let waitForTrial = circuitBreakerPhases s >>= \p -> if p == [BreakerHalfOpen] then pure () else waitForTrial
      waitForTrial
      callAll breaker [1] `shouldReturn` [openFailure]
      atomically $ putTMVar gate ()
      wait trial `shouldReturn` 7
      isCircuitBreakerClosed s `shouldReturn` True

    it "A failed trial opens the circuit again for the open duration" $ do
      vc  <- newVirtualClock
      ref <- newIORef 0
      (s, breaker) <- circuitBreaker ((options vc) { breakerFailureThreshold = 1, breakerOpenDurationMs = 500 }) (failing ref)
      _ <- callAll breaker [1]
      advanceClock vc 500000
      callAll breaker [1, 1] `shouldReturn` [Right (Left CircuitBreakerTestException), openFailure]
      circuitBreakerPhases s `shouldReturn` [BreakerOpen]
      readIORef ref `shouldReturn` 2

    it "Failures rejected by countsAsFailure do not count" $ do
      vc <- newVirtualClock
      let counts e = maybe True (const False) (fromException e :: Maybe BadRequestException)
      (s, breaker) <- circuitBreaker ((options vc) { breakerFailureThreshold = 1, countsAsFailure = counts }) $ \(req :: Int) ->
        throwIO (BadRequestException req) :: IO Int
      results <- forM [1 .. 5] (try . breaker)
      results `shouldBe` fmap (Left . BadRequestException) [1 .. 5]
      isCircuitBreakerClosed s `shouldReturn` True

    it "Reports each transition to the observer" $ do
      vc  <- newVirtualClock
      seen <- newIORef []
      ref <- newIORef 0
      healthy <- newIORef False
      let service (req :: Int) = readIORef healthy >>= \ok -> if ok then pure req else failing ref req
          observe t = atomicModifyIORef' seen (\ts -> (ts ++ [t], ()))
      (_, breaker) <- circuitBreaker ((options vc) { breakerFailureThreshold = 2, breakerOpenDurationMs = 10, onBreakerTransition = observe }) service
      _ <- callAll breaker [1, 1]
      advanceClock vc 10000
      _ <- callAll breaker [1]
      advanceClock vc 10000
      writeIORef healthy True
      _ <- callAll breaker [1]
      readIORef seen `shouldReturn`
        [ BreakerTransition BreakerClosed BreakerOpen
        , BreakerTransition BreakerOpen BreakerHalfOpen
        , BreakerTransition BreakerHalfOpen BreakerOpen
        , BreakerTransition BreakerOpen BreakerHalfOpen
        , BreakerTransition BreakerHalfOpen BreakerClosed
        ]

    it "combineCircuitBreakerStates reports open if any breaker is open" $ do
      let opts = (defaultCircuitBreakerOptions :: CircuitBreakerOptions IO e) { breakerFailureThreshold = 1 }
      (s1, svc1) <- circuitBreaker opts (\(_ :: Int) -> throwIO CircuitBreakerTestException :: IO Int)
      (s2, _)    <- circuitBreaker opts (\(x :: Int) -> pure (x + 1) :: IO Int)
      let combined = combineCircuitBreakerStates [s1, s2]
      isCircuitBreakerOpen combined `shouldReturn` False
      isCircuitBreakerClosed combined `shouldReturn` True
      _ <- try (svc1 1) :: IO (Either CircuitBreakerTestException Int)
      isCircuitBreakerOpen combined `shouldReturn` True
      isCircuitBreakerClosed combined `shouldReturn` False

  describe "circuitBreakerWithStore" $
    it "Never calls the service when the store refuses" $ do
      ref <- newIORef 0
      let store = BreakerStore (pure (Refused, Nothing)) (\_ _ -> pure Nothing) (pure BreakerOpen)
          breaker = circuitBreakerWithStore defaultCircuitBreakerOptions store (failing ref)
      callAll breaker [1, 2] `shouldReturn` [openFailure, openFailure]
      readIORef ref `shouldReturn` 0

  describe "circuitBreakerEither" $ do
    it "Counts Left failures and refuses with the given value" $ do
      vc <- newVirtualClock
      (s, breaker) <- circuitBreakerEither ((options vc) { breakerFailureThreshold = 2 }) "open" $ \(_ :: Int) ->
        pure (Left "down" :: Either String Int)
      traverse breaker [1, 2, 3] `shouldReturn` [Left "down", Left "down", Left "open"]
      isCircuitBreakerOpen s `shouldReturn` True

    it "Behaves like the exception shape for the same outcomes" $
      property $ \(steps :: [Step]) -> do
        vc1 <- newVirtualClock
        vc2 <- newVirtualClock
        let opts vc = (options vc) { breakerFailureThreshold = 2, breakerOpenDurationMs = 20 }
            countsException e = maybe True (\(StepFailure c) -> c) (fromException e)
            asException (Fail c) = throwIO (StepFailure c)
            asException step     = pure (stepValue step)
            asValue (Fail c) = pure (Left (Just c))
            asValue step     = pure (Right (stepValue step))
        (_, exceptional) <- circuitBreaker ((opts vc1) { countsAsFailure = countsException }) asException
        (_, valued)      <- circuitBreakerEither ((opts vc2) { countsAsFailure = or }) Nothing asValue
        let run step = do
              e <- try (try (exceptional step))
              v <- valued step
              case step of
                Wait n -> advanceClock vc1 (n * 1000) >> advanceClock vc2 (n * 1000)
                _      -> pure ()
              let normalised = case e of
                    Left (CircuitBreakerException _) -> Left Nothing
                    Right (Left (StepFailure c))     -> Left (Just c)
                    Right (Right n)                  -> Right n
              pure (normalised, v)
        pairs <- traverse run steps
        fmap fst pairs `shouldBe` fmap snd pairs
