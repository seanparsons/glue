{-# LANGUAGE ScopedTypeVariables #-}

module Glue.FailureSpec (spec) where

import           Control.Exception          (AsyncException (UserInterrupt))
import qualified Control.Exception          as E
import           Control.Monad.Trans.Except (runExceptT, throwE)
import           Glue.Failure
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception         (Exception, fromException, throwIO,
                                             try)

data FailureTestException = FailureTestException
  deriving (Eq, Show)

instance Exception FailureTestException

spec :: Spec
spec = do
  describe "exceptionFailures" $ do
    it "Observes synchronous exceptions as failures" $ do
      result <- observeCall exceptionFailures (throwIO FailureTestException :: IO Int)
      either fromException (const Nothing) result `shouldBe` Just FailureTestException

    it "Passes successes through" $
      property $ \(x :: Int) ->
        fmap (either (const Nothing) Just) (observeCall exceptionFailures (pure x)) `shouldReturn` Just x

    it "Never catches asynchronous exceptions" $
      observeCall exceptionFailures (E.throwIO UserInterrupt :: IO Int) `shouldThrow` (== UserInterrupt)

  describe "valueFailures" $ do
    it "Observes Left as the failure and Right as the success" $
      property $ \(r :: Either String Int) ->
        observeCall valueFailures (pure r) `shouldReturn` either Left (const (Right r)) r

    it "Reraises as a Left value" $
      (reraise valueFailures "bad" :: IO (Either String Int)) `shouldReturn` Left "bad"

    it "Leaves thrown exceptions alone" $ do
      result <- try (observeCall valueFailures (throwIO FailureTestException :: IO (Either String Int)))
      either (\(e :: FailureTestException) -> Just e) (const Nothing) result `shouldBe` Just FailureTestException

  describe "ExceptT conversion" $
    it "Round trips through EitherService" $
      property $ \(x :: Int) -> do
        let service n = if n > 0 then pure n else throwE ("non-positive" :: String)
        fromExceptTService service x `shouldReturn` (if x > 0 then Right x else Left "non-positive")
        runExceptT (toExceptTService (fromExceptTService service) x) `shouldReturn` (if x > 0 then Right x else Left "non-positive")
