{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, ScopedTypeVariables #-}

module Glue.CachingSpec where

import Data.Typeable
import Glue.Caching
import Test.Hspec
import Data.IORef
import Test.QuickCheck
import Control.Exception.Base hiding (throw)
import Control.Monad.CatchIO
import Data.Cache.LRU.IO

data CachingTestException = CachingTestException deriving (Eq, Show, Typeable)
instance Exception CachingTestException

spec :: Spec
spec = do
  describe "cacheWithAtomicLRU" $ do
    it "For a second request, the value comes from the cache" $ do
      property $ \(request :: Int, result :: Int) -> do
        ref <- newIORef (0 :: Int)
        let service req = do
                            if req == request then return () else throw CachingTestException
                            callCount <- atomicModifyIORef' ref (\c -> (c + 1, c + 1))
                            if callCount > 1 then throw CachingTestException else return result
        cache <- newAtomicLRU $ Just 100
        let cachedService = cacheWithAtomicLRU cache service
        (cachedService request) `shouldReturn` result
        (cachedService request) `shouldReturn` result


