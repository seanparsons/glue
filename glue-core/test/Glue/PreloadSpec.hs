{-# LANGUAGE ScopedTypeVariables #-}

module Glue.PreloadSpec (spec) where

import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Glue.Preload
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()
import           UnliftIO.Exception        (Exception, finally, throwIO)

serviceFunctionality :: MultiGetRequest Int -> MultiGetResponse Int Int
serviceFunctionality rs = M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList rs

failingService :: MultiGetRequest Int -> IO (MultiGetResponse Int Int)
failingService _ = throwIO PreloadTestException

successfulCall :: IO Int
successfulCall = pure 12

failingCall :: IO Int
failingCall = throwIO PreloadTestException

data PreloadTestException = PreloadTestException
  deriving (Eq, Show)

instance Exception PreloadTestException

spec :: Spec
spec = do
  describe "preloadingService" $ do
    it "Requests should work the same regardless of whether or not parts are preloaded" $
      property $ \(preload :: S.HashSet Int, nonPreload :: S.HashSet Int) -> do
        let service = (pure . serviceFunctionality) :: MultiGetService IO Int Int
        (preloadedService, disable) <- preloadingService (defaultPreloadedOptions preload id) service
        let expectedResults = serviceFunctionality (S.union preload nonPreload)
        actualResults <- preloadedService (S.union preload nonPreload) `finally` disable ()
        actualResults `shouldBe` expectedResults

    it "Exceptions should propagate" $
      property $ \(preload :: S.HashSet Int, nonPreload :: S.HashSet Int) -> do
        (preloadedService, disable) <- preloadingService (defaultPreloadedOptions preload id) failingService
        let expectedResults = serviceFunctionality (S.union preload nonPreload)
            goodPath = preloadedService (S.union preload nonPreload) `shouldReturn` expectedResults
            badPath  = preloadedService (S.union preload nonPreload) `shouldThrow` (== PreloadTestException)
        (if S.null preload && S.null nonPreload then goodPath else badPath) `finally` disable ()

  describe "preloadingCall" $ do
    it "Requests should work" $
      property $ do
        (preloadedCall, disable) <- preloadingCall id 1000 successfulCall
        actualResults <- preloadedCall `finally` disable ()
        actualResults `shouldBe` 12

    it "Exceptions should propagate" $
      property $ do
        (preloadedCall, disable) <- preloadingCall id 1000 failingCall
        (preloadedCall `shouldThrow` (== PreloadTestException)) `finally` disable ()
