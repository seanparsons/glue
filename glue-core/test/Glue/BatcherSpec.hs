{-# LANGUAGE ScopedTypeVariables #-}

module Glue.BatcherSpec (spec) where

import           Control.Concurrent.Async (mapConcurrently)
import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as S
import           Glue.Batcher
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "batchingService" $ do
    it "Multi-get requests should receive their full results" $
      property $ \(requests :: [[Int]]) -> do
        let requestsAsSets = fmap S.fromList requests
            serviceFunctionality rs = M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList rs
            service = (pure . serviceFunctionality) :: MultiGetService IO Int Int
        (_, multiService) <- batchingService (BatchingOptions 2) service
        let expectedResults = fmap serviceFunctionality requestsAsSets
        mapConcurrently multiService requestsAsSets `shouldReturn` expectedResults

    it "Single-key requests should receive their individual lookup results" $
      property $ \(requests :: [Int]) -> do
        let serviceFunctionality rs = M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList rs
            service = (pure . serviceFunctionality) :: MultiGetService IO Int Int
        (singleService, _) <- batchingService (BatchingOptions 2) service
        let expectedResults = fmap (\r -> Just (r * 2)) requests
        mapConcurrently singleService requests `shouldReturn` expectedResults
