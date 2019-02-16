{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.BatcherSpec where

import           Control.Concurrent.Async
import qualified Data.HashMap.Strict      as M
import qualified Data.HashSet             as S
import           Glue.Batcher
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "batchingService" $ do
    it "Requests should receive their full results" $ do
      property $ \(requests :: [[Int]]) -> do
        let requestsAsSets = fmap (S.fromList) requests
        let serviceFunctionality rs = M.fromList $ fmap (\r -> (r, r * 2)) $ S.toList rs
        let service = (return . serviceFunctionality) :: MultiGetService IO Int Int
        (_, multiService) <- batchingService defaultBatchingOptions service
        let expectedResults = fmap serviceFunctionality requestsAsSets
        (mapConcurrently multiService requestsAsSets) `shouldReturn` expectedResults

