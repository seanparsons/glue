{-# LANGUAGE ScopedTypeVariables #-}

module Glue.LoadBalancerSpec (spec) where

import           Control.Concurrent.Async (mapConcurrently)
import           Data.IORef
import           Data.List.NonEmpty       (NonEmpty (..))
import           Glue.LoadBalancer
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck

spec :: Spec
spec = do
  describe "roundRobinLoadBalancer" $ do
    it "Throws NoServicesAvailableException when given an empty list" $ do
      lb <- roundRobinLoadBalancer ([] :: [BasicService IO Int Int])
      lb 42 `shouldThrow` (== NoServicesAvailableException)

    it "Distributes requests evenly in round-robin order" $
      property $ \(reqs :: [Int]) -> do
        ref0 <- newIORef (0 :: Int)
        ref1 <- newIORef (0 :: Int)
        ref2 <- newIORef (0 :: Int)
        let mkSvc tag ref req = do
              atomicModifyIORef' ref (\c -> (c + 1, ()))
              pure (tag, req * 2)
        lb <- roundRobinLoadBalancerNE
          (  mkSvc (0 :: Int) ref0
          :| [mkSvc 1 ref1, mkSvc 2 ref2]
          )
        results <- traverse lb reqs
        let expectedTags = zipWith (\i r -> (i `mod` 3, r * 2)) [0 ..] reqs
        results `shouldBe` expectedTags

    it "Handles concurrent requests across all backends" $ do
      ref0 <- newIORef (0 :: Int)
      ref1 <- newIORef (0 :: Int)
      let svc ref req = atomicModifyIORef' ref (\c -> (c + 1, ())) >> pure (req + 1)
      lb <- roundRobinLoadBalancer [svc ref0, svc ref1]
      results <- mapConcurrently lb ([1 .. 20] :: [Int])
      results `shouldBe` [2 .. 21]
      c0 <- readIORef ref0
      c1 <- readIORef ref1
      c0 `shouldBe` 10
      c1 `shouldBe` 10
