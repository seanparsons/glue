{-# LANGUAGE ScopedTypeVariables #-}

module Glue.WeightedLoadBalancerSpec (spec) where

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Either           (isRight)
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.Map.Strict       as M
import           Glue.LoadBalancer
import           Test.Hspec
import           Test.QuickCheck
import           UnliftIO.Exception    (Exception, throwIO)

data BackendDown = BackendDown
  deriving (Eq, Show)

instance Exception BackendDown

referenceKey :: (Integer, Integer)
referenceKey = (0x0706050403020100, 0x0f0e0d0c0b0a0908)

referenceHash :: Int -> Integer
referenceHash n = toInteger (sipHash24 (fromInteger (fst referenceKey)) (fromInteger (snd referenceKey)) (BS.pack (fmap fromIntegral [0 .. n - 1])))

keys :: [BS.ByteString]
keys = fmap (BC.pack . ("session-" ++) . show) [1 .. 20000 :: Int]

spec :: Spec
spec = do
  describe "sipHash24" $
    it "Matches the reference vectors (key 00..0f, message 00..n-1)" $ do
      referenceHash 0  `shouldBe` 0x726fdb47dd0e0e31
      referenceHash 1  `shouldBe` 0x74f839c593dc67fd
      referenceHash 7  `shouldBe` 0xab0200f58b01d137
      referenceHash 8  `shouldBe` 0x93f5f5799a932462
      referenceHash 15 `shouldBe` 0xa129ca6149be45e5
      referenceHash 63 `shouldBe` 0x958a324ceb064572

  describe "validateWeights" $ do
    it "Accepts non-negative weights summing to 10000, including zero weights" $ do
      validateWeights (10000 :| []) `shouldBe` Right ()
      validateWeights (8000 :| [2000, 0]) `shouldBe` Right ()

    it "Rejects negative weights and wrong sums" $ do
      validateWeights (10500 :| [-500]) `shouldBe` Left (NegativeWeight 1 (-500))
      validateWeights (5000 :| [4000]) `shouldBe` Left (WeightsDoNotSumToTotal 9000)

  describe "chooseIndex" $ do
    it "Is deterministic for a key and salt" $
      property $ \(key :: String) -> do
        let salt = saltFromBytes (BC.pack "split")
        chooseIndex salt (BC.pack key) (5000 :| [5000]) `shouldBe` chooseIndex salt (BC.pack key) (5000 :| [5000])

    it "Never chooses a zero-weight backend and gives a full-weight backend everything" $
      property $ \(key :: String) -> do
        let salt = saltFromBytes (BC.pack "split")
        chooseIndex salt (BC.pack key) (0 :| [10000, 0]) `shouldBe` 1
        chooseIndex salt (BC.pack key) (10000 :| [0]) `shouldBe` 0

    it "Maps buckets to backends by their cumulative ranges" $
      property $ \(key :: String) -> do
        let salt   = saltFromBytes (BC.pack "split")
            bucket = bucketOf salt (BC.pack key)
            index  = chooseIndex salt (BC.pack key) (2500 :| [2500, 5000])
        index `shouldBe` (if bucket < 2500 then 0 else if bucket < 5000 then 1 else 2)

    it "Keeps a key's backend when unrelated weights change order" $
      property $ \(key :: String) -> do
        let salt   = saltFromBytes (BC.pack "split")
            original = chooseIndex salt (BC.pack key) (5000 :| [3000, 2000])
            reordered = chooseIndex salt (BC.pack key) (5000 :| [2000, 3000])
        -- Keys in the first backend's range are unaffected by reordering the others.
        (original == 0) `shouldBe` (reordered == 0)

    it "Splits traffic in proportion to the weights" $ do
      let salt   = saltFromBytes (BC.pack "split")
          counts = M.fromListWith (+) [(chooseIndex salt key (8000 :| [2000]), 1 :: Int) | key <- keys]
          share index = fromIntegral (M.findWithDefault 0 index counts) / fromIntegral (length keys) :: Double
      share 0 `shouldSatisfy` (\s -> abs (s - 0.8) < 0.015)
      share 1 `shouldSatisfy` (\s -> abs (s - 0.2) < 0.015)

    it "Gives independent assignments for different salts" $ do
      let a = saltFromBytes (BC.pack "split-a")
          b = saltFromBytes (BC.pack "split-b")
          both = length [() | key <- keys, chooseIndex a key (5000 :| [5000]) == 0, chooseIndex b key (5000 :| [5000]) == 0]
          ratio = fromIntegral both / fromIntegral (length keys) :: Double
      ratio `shouldSatisfy` (\r -> abs (r - 0.25) < 0.015)

  describe "weightedBy" $ do
    it "Rejects invalid weights up front" $ do
      let backends = (6000, pure . (+ 1)) :| [(6000, pure . (+ 2))] :: NonEmpty (Int, Int -> IO Int)
      either Just (const Nothing) (weightedBy (BC.pack . show) (Salt 1 2) backends) `shouldBe` Just (WeightsDoNotSumToTotal 12000)

    it "Routes every request to the backend chooseIndex picks" $
      property $ \(request :: Int) -> do
        let salt     = Salt 1 2
            backends = (3000, \r -> pure (0, r)) :| [(7000, \r -> pure (1, r))] :: NonEmpty (Int, Int -> IO (Int, Int))
            keyOf    = BC.pack . show
        case weightedBy keyOf salt backends of
          Left err      -> expectationFailure (show err)
          Right service -> service request `shouldReturn` (chooseIndex salt (keyOf request) (3000 :| [7000]), request)

    it "Propagates a failure of the chosen backend" $ do
      let backends = (10000, \_ -> throwIO BackendDown) :| [(0, pure)] :: NonEmpty (Int, Int -> IO Int)
          built    = weightedBy (BC.pack . show) (Salt 1 2) backends
      isRight built `shouldBe` True
      either (const (pure ())) (\service -> service 1 `shouldThrow` (== BackendDown)) built
