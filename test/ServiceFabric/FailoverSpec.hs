{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module ServiceFabric.FailoverSpec where

import Data.Typeable
import ServiceFabric.Failover
import Test.Hspec
import Test.QuickCheck
import Control.Exception.Base hiding (throw)
import Control.Monad.CatchIO

data FailoverTestException = FailoverTestException deriving (Eq, Show, Typeable)
instance Exception FailoverTestException

spec :: Spec
spec = do
  describe "failover" $ do
    it "Failover handles potentially multiple errors" $ do
      property $ \request ->
        let
          service req       = if req >= 10 then return (req + 100) else throw FailoverTestException 
          options           = defaultFailoverOptions { transformRequest = (+1) }
          failOverService   = failover options service
          successCase       = (failOverService request) `shouldReturn` ((if request <= 10 then 10 else request) + 100)
          failureCase       = (failOverService request) `shouldThrow` (== FailoverTestException)
        in if (request :: Int) >= 7 then successCase else failureCase 