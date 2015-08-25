{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Glue.CachingSpec where

import Data.Typeable
import Glue.Caching
import Glue.Types
import Test.QuickCheck.Instances()
import Test.Hspec
import Data.IORef
import Test.QuickCheck
import Control.Exception.Base hiding (throw, throwIO)
import Control.Exception.Lifted
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

data CachingTestException = CachingTestException Int deriving (Eq, Show, Typeable)
instance Exception CachingTestException

type TestResp = MultiGetResponse Int Int

spec :: Spec
spec = do
  describe "cacheWithBasic" $ do
    it "For a second request, the value comes from the cache" $ do
      property $ \(request :: Int, result :: Int) -> do
        ref <- newIORef (0 :: Int)
        let service req = do
                            if req == request then return () else throwIO (CachingTestException (-1))
                            callCount <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
                            if callCount > 1 then throwIO (CachingTestException callCount) else return result
        cache <- newIORef M.empty
        let lookupWith r = fmap (M.lookup r) $ readIORef cache
        let insertWith req resp = atomicModifyIORef' cache (\c -> (M.insert req resp c, ()))
        let cachedService = cacheWithBasic lookupWith insertWith service
        (cachedService request) `shouldReturn` result
        (cachedService request) `shouldReturn` result
  describe "cacheWithMulti" $ do
    it "For a second request, the value comes from the cache" $ do
      property $ \(result :: TestResp) -> do
        let request = S.fromList $ M.keys result
        ref <- newIORef (0 :: Int)
        let service req = do
                            if req == request then return () else throwIO (CachingTestException (-1))
                            callCount <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
                            if callCount > 1 then throwIO (CachingTestException callCount) else return result
        cache <- newIORef M.empty
        let lookupWith rs = fmap (M.filterWithKey (\k -> \_ -> S.member k rs)) $ readIORef cache
        let insertWith resp = atomicModifyIORef' cache (\c -> (M.union resp c, ()))
        let cachedService = cacheWithMulti lookupWith insertWith service
        (cachedService request) `shouldReturn` result
        (cachedService request) `shouldReturn` result
    it "Merges the cached values as appropriate from subsequent requests" $ do
      property $ \(first :: TestResp, second :: TestResp, both :: TestResp) -> do
        let uniqueFirst = M.difference first (M.union second both)
        let uniqueSecond = M.difference second (M.union first both)
        let uniqueBoth = M.difference both (M.union first second)
        let uniqueAll = M.union uniqueFirst $ M.union uniqueSecond uniqueBoth
        let firstResult = M.union uniqueFirst uniqueBoth
        let secondResult = M.union uniqueSecond uniqueBoth
        let firstRequest = S.fromList $ M.keys firstResult
        let secondRequest = S.fromList $ M.keys secondResult
        ref <- newIORef (0 :: Int)
        let service req = do
                            callCount <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
                            if callCount > 2 then throwIO (CachingTestException callCount) else return $ M.filterWithKey (\k -> \_ -> S.member k req) uniqueAll
        cache <- newIORef M.empty
        let lookupWith rs = fmap (M.filterWithKey (\k -> \_ -> S.member k rs)) $ readIORef cache
        let insertWith resp = atomicModifyIORef' cache (\c -> (M.union resp c, ()))
        let cachedService = cacheWithMulti lookupWith insertWith service
        (cachedService firstRequest) `shouldReturn` firstResult
        (cachedService secondRequest) `shouldReturn` secondResult
        (cachedService firstRequest) `shouldReturn` firstResult
        (cachedService secondRequest) `shouldReturn` secondResult


