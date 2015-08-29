{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Glue.PreloadSpec where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Glue.Preload
import Glue.Types
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()
import Control.Exception.Base
import Data.Typeable

serviceFunctionality :: MultiGetRequest Int -> MultiGetResponse Int Int
serviceFunctionality rs = M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList rs

failingService :: MultiGetRequest Int -> IO (MultiGetResponse Int Int)
failingService _ = throwIO PreloadTestException

data PreloadTestException = PreloadTestException deriving (Eq, Show, Typeable)
instance Exception PreloadTestException

handler :: SomeException -> IO (M.HashMap Int Int)
handler e = print e >> return M.empty

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