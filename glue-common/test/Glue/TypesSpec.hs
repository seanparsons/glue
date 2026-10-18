{-# LANGUAGE ScopedTypeVariables #-}

module Glue.TypesSpec (spec) where

import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           UnliftIO.Exception        (Exception, SomeException (..),
                                            fromException, throwIO)
import           UnliftIO.MVar             (newMVar)

data TypesTestException = TypesTestException
  deriving (Eq, Show)

instance Exception TypesTestException

spec :: Spec
spec = do
  describe "multiGetToBasic and basicToMultiGet" $ do
    it "basicToMultiGet lifts a BasicService over a HashSet of requests" $
      property $ \(requests :: S.HashSet Int) -> do
        let basic :: BasicService IO Int Int
            basic x = pure (x * 2)
            multi = basicToMultiGet basic
            expected = M.fromList [ (k, k * 2) | k <- S.toList requests ]
        multi requests `shouldReturn` expected

    it "multiGetToBasic looks up a single key from a MultiGetService" $
      property $ \(resp :: MultiGetResponse Int Int, key :: Int) -> do
        let multi :: MultiGetService IO Int Int
            multi _ = pure resp
            basic = multiGetToBasic multi
        basic key `shouldReturn` M.lookup key resp

    it "multiGetToBasic . basicToMultiGet round-trips as Just" $
      property $ \(key :: Int) -> do
        let basic :: BasicService IO Int Int
            basic x = pure (x + 10)
            roundTripped = multiGetToBasic (basicToMultiGet basic)
        roundTripped key `shouldReturn` Just (key + 10)

  describe "getResult" $ do
    it "returns the value when ResultVar holds Right" $
      property $ \(val :: Int) -> do
        var <- newMVar (Right val)
        getResult var `shouldReturn` val

    it "rethrows the exception when ResultVar holds Left" $ do
      var <- newMVar (Left (SomeException TypesTestException) :: Either SomeException Int)
      getResult var `shouldThrow` (== TypesTestException)

  describe "makeCall" $ do
    it "returns Right on a successful multi-get call" $
      property $ \(requests :: S.HashSet Int) -> do
        let expected = M.fromList [ (k, k * 3) | k <- S.toList requests ]
            service :: MultiGetService IO Int Int
            service _ = pure expected
        result <- makeCall service requests
        case result of
          Right actual -> actual `shouldBe` expected
          Left err     -> expectationFailure $ "Unexpected failure: " <> show err

    it "captures synchronous exceptions in Left" $ do
      let service :: MultiGetService IO Int Int
          service _ = throwIO TypesTestException
      result <- makeCall service (S.singleton 1)
      case result of
        Left err -> fromException err `shouldBe` Just TypesTestException
        Right v  -> expectationFailure $ "Expected Left exception, got Right: " <> show v
