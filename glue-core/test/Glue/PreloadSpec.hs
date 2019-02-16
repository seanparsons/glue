{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.PreloadSpec where

import           Control.Exception.Base
import qualified Data.HashMap.Strict       as M
import qualified Data.HashSet              as S
import           Data.Typeable
import           Glue.Preload
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

serviceFunctionality :: MultiGetRequest Int -> MultiGetResponse Int Int
serviceFunctionality rs = M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList rs

failingService :: MultiGetRequest Int -> IO (MultiGetResponse Int Int)
failingService _ = throwIO PreloadTestException

successfullCall :: IO Int
successfullCall = return 12

failingCall :: IO Int
failingCall = throwIO PreloadTestException

data PreloadTestException = PreloadTestException deriving (Eq, Show, Typeable)
instance Exception PreloadTestException

spec :: Spec
spec = do
  describe "preloadingService" $ do
    it "Requests should work the same regardless of whether or not parts are preloaded" $ do
      property $ \(preload :: S.HashSet Int, nonPreload :: S.HashSet Int) -> do
        let service = (return . serviceFunctionality) :: MultiGetService IO Int Int
        (preloadedService, disable) <- preloadingService (defaultPreloadedOptions preload id) service
        let expectedResults = serviceFunctionality (S.union preload nonPreload)
        actualResults <- preloadedService (S.union preload nonPreload)
        disable ()
        actualResults `shouldBe` expectedResults
    it "Exceptions should propagate" $ do
      property $ \(preload :: S.HashSet Int, nonPreload :: S.HashSet Int) -> do
        (preloadedService, _) <- preloadingService (defaultPreloadedOptions preload id) failingService
        let expectedResults = serviceFunctionality (S.union preload nonPreload)
        let goodPath = preloadedService (S.union preload nonPreload) `shouldReturn` expectedResults
        let badPath = preloadedService (S.union preload nonPreload) `shouldThrow` (== PreloadTestException)
        if (S.null preload && S.null nonPreload) then goodPath else badPath
  describe "preloadingCall" $ do
    it "Requests should work" $ do
      property $ do
        (preloadedCall, disable) <- preloadingCall id 1000 successfullCall
        actualResults <- preloadedCall
        disable ()
        actualResults `shouldBe` 12
    it "Exceptions should propagate" $ do
      property $ do
        (preloadedCall, _) <- preloadingCall id 1000 failingCall
        preloadedCall `shouldThrow` (== PreloadTestException)
