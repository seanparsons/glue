{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Glue.SwitchingSpec where

import           Glue.Switching
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  describe "switchingService" $ do
    it "Requests should be handled by the service used" $ do
      property $ \(firstResult :: MultiGetResponse Int Int, secondResult :: MultiGetResponse Int Int, request :: MultiGetRequest Int) -> do
        let first _ = return firstResult
        let second _ = return secondResult
        (switchedService, serviceUpdate) <- switchingService first
        actualFirstResult <- switchedService request
        expectedFirstResult <- first request
        serviceUpdate second
        actualSecondResult <- switchedService request
        expectedSecondResult <- second request
        actualFirstResult `shouldBe` expectedFirstResult
        actualSecondResult `shouldBe` expectedSecondResult
