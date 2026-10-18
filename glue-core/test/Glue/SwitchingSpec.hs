{-# LANGUAGE ScopedTypeVariables #-}

module Glue.SwitchingSpec (spec) where

import           Glue.Switching
import           Glue.Types
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

spec :: Spec
spec = do
  describe "switchingService" $ do
    it "Requests should be handled by the service used" $
      property $ \(firstResult :: MultiGetResponse Int Int, secondResult :: MultiGetResponse Int Int, request :: MultiGetRequest Int) -> do
        let first _  = pure firstResult
            second _ = pure secondResult
        (switchedService, serviceUpdate) <- switchingService first
        actualFirstResult   <- switchedService request
        expectedFirstResult <- first request
        serviceUpdate second
        actualSecondResult   <- switchedService request
        expectedSecondResult <- second request
        actualFirstResult `shouldBe` expectedFirstResult
        actualSecondResult `shouldBe` expectedSecondResult
