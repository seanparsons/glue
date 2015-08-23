{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Glue.PreloadSpec where

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Glue.Preload
import Glue.Types
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances()

serviceFunctionality :: MultiGetRequest Int -> MultiGetResponse Int Int
serviceFunctionality rs = M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList rs

spec :: Spec
spec = do
  describe "preloadingService" $ do
    it "Requests should work the same regardless of whether or not parts are preloaded" $ do
      property $ \(preload :: S.HashSet Int, nonPreload :: S.HashSet Int) -> do
        let service = (return . serviceFunctionality) :: MultiGetService IO Int Int
        (preloadedService, disable) <- preloadingService (defaultPreloadedOptions preload) service
        let expectedResults = serviceFunctionality (S.union preload nonPreload)
        actualResults <- preloadedService (S.union preload nonPreload)
        disable ()
        actualResults `shouldBe` expectedResults
