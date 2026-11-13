{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Module providing load balancing across multiple services: round-robin, or a deterministic
-- weighted split keyed by a stable request key.
module Glue.LoadBalancer
  ( NoServicesAvailableException (..)
  , roundRobinLoadBalancer
  , roundRobinLoadBalancerNE
    -- * Weighted, keyed balancing
  , Weight
  , totalWeight
  , WeightError (..)
  , Salt (..)
  , saltFromBytes
  , validateWeights
  , bucketOf
  , chooseIndex
  , weightedBy
  , sipHash24
  ) where

import           Data.Bits          (rotateL, shiftL, xor, (.|.))
import qualified Data.ByteString    as BS
import           Data.Foldable      (toList)
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import           Data.Word          (Word64)
import           Glue.Types
import           UnliftIO           (Exception, MonadIO)
import           UnliftIO.Exception (throwIO)
import           UnliftIO.IORef     (atomicModifyIORef', newIORef)

-- | Exception thrown when attempting to call a load balancer configured with an empty set of services.
data NoServicesAvailableException = NoServicesAvailableException
  deriving (Eq, Show)

instance Exception NoServicesAvailableException

-- | Creates a service that distributes requests across a 'NonEmpty' list of backend services in round-robin order.
roundRobinLoadBalancerNE
  :: (MonadIO m, MonadIO n)
  => NonEmpty (BasicService m a b)
  -- ^ Non-empty collection of backend services to balance across.
  -> n (BasicService m a b)
roundRobinLoadBalancerNE services = do
  let svcList = NE.toList services
      count   = length svcList
  indexRef <- newIORef (0 :: Int)
  let balanced request = do
        idx <- atomicModifyIORef' indexRef (\i -> ((i + 1) `mod` count, i))
        let target = svcList !! idx
        target request
  pure balanced

-- | Creates a service that distributes requests across a foldable collection of backend services in round-robin order.
-- If the collection is empty, invoking the returned service throws t'NoServicesAvailableException'.
roundRobinLoadBalancer
  :: (MonadIO m, MonadIO n, Foldable t)
  => t (BasicService m a b)
  -- ^ Collection of backend services to balance across.
  -> n (BasicService m a b)
roundRobinLoadBalancer services =
  case toList services of
    []     -> pure $ const $ throwIO NoServicesAvailableException
    (x:xs) -> roundRobinLoadBalancerNE (x :| xs)

-- | A backend's share of traffic in basis points: the weights of a split sum to 'totalWeight'.
type Weight = Int

-- | The sum every set of weights must have: 10000 basis points.
totalWeight :: Int
totalWeight = 10000

-- | Why a set of weights was rejected.
data WeightError
  = NegativeWeight Int Weight
  -- ^ The weight at this index is negative.
  | WeightsDoNotSumToTotal Int
  -- ^ The weights sum to this rather than 'totalWeight'.
  deriving (Eq, Show)

-- | A 128-bit SipHash key. Different salts give independent assignments, so two splits keyed on
-- the same request key never correlate.
data Salt = Salt Word64 Word64
  deriving (Eq, Show)

-- | Derives a salt from arbitrary bytes (a split's name, say) by hashing them under two fixed keys.
saltFromBytes :: BS.ByteString -> Salt
saltFromBytes bytes = Salt (sipHash24 0 0 bytes) (sipHash24 0 1 bytes)

-- | Checks that no weight is negative and that they sum to 'totalWeight'.
validateWeights :: NonEmpty Weight -> Either WeightError ()
validateWeights weights =
  case [(index, w) | (index, w) <- zip [0 ..] (toList weights), w < 0] of
    (index, w) : _ -> Left (NegativeWeight index w)
    []
      | sum weights /= totalWeight -> Left (WeightsDoNotSumToTotal (sum weights))
      | otherwise                  -> Right ()

-- | The bucket, from 0 to 'totalWeight' - 1, that a key falls in for a salt: the key's SipHash-2-4
-- under the salt, modulo 'totalWeight'. Reducing a 64-bit hash this way favours the lowest
-- @2^64 mod 10000 = 1616@ buckets by one value in about @1.8 * 10^15@, a negligible bias.
-- The bucket never depends on the weights.
bucketOf :: Salt -> BS.ByteString -> Int
bucketOf (Salt k0 k1) key = fromIntegral (sipHash24 k0 k1 key `mod` fromIntegral totalWeight)

-- | The index of the backend a key is assigned to: backends own consecutive ranges of buckets in
-- list order, sized by their weights. A key keeps its backend across instances and restarts, and
-- changing weights only moves the keys whose bucket changes owner. Weights should satisfy
-- 'validateWeights'; buckets beyond their sum go to the last backend.
chooseIndex :: Salt -> BS.ByteString -> NonEmpty Weight -> Int
chooseIndex salt key weights = go 0 0 (toList weights)
  where
    bucket = bucketOf salt key
    go index _ [_] = index
    go index start (w : ws)
      | bucket < start + max 0 w = index
      | otherwise                = go (index + 1) (start + max 0 w) ws
    go index _ [] = index

-- | Creates a service splitting traffic between backends by weight, choosing each request's backend
-- from a stable key (a session id, else a request id) so the same key always reaches the same
-- backend. A failure of the chosen backend propagates; it is never redistributed (compose with
-- 'Glue.Failover.failoverAcross' for that).
weightedBy
  :: (a -> BS.ByteString)
  -- ^ The request's stable key.
  -> Salt
  -- ^ The split's salt.
  -> NonEmpty (Weight, BasicService m a b)
  -- ^ Backends with their weights in basis points.
  -> Either WeightError (BasicService m a b)
weightedBy keyOf salt backends = do
  let weights  = fmap fst backends
      services = fmap snd backends
  validateWeights weights
  pure $ \request -> (services NE.!! chooseIndex salt (keyOf request) weights) request

-- | SipHash-2-4 of the bytes under the 128-bit key given as two little-endian words.
sipHash24 :: Word64 -> Word64 -> BS.ByteString -> Word64
sipHash24 k0 k1 message = finish (compress (initial, message))
  where
    initial = (k0 `xor` 0x736f6d6570736575, k1 `xor` 0x646f72616e646f6d, k0 `xor` 0x6c7967656e657261, k1 `xor` 0x7465646279746573)
    len = BS.length message
    compress (!v, bytes)
      | BS.length bytes >= 8 = compress (absorb v (word (BS.take 8 bytes)), BS.drop 8 bytes)
      | otherwise = absorb v (word bytes .|. (fromIntegral (len `mod` 256) `shiftL` 56))
    absorb (v0, v1, v2, v3) m =
      let (v0', v1', v2', v3') = sipRound (sipRound (v0, v1, v2, v3 `xor` m))
      in  (v0' `xor` m, v1', v2', v3')
    finish (v0, v1, v2, v3) =
      let (v0', v1', v2', v3') = iterate sipRound (v0, v1, v2 `xor` 0xff, v3) !! 4
      in  v0' `xor` v1' `xor` v2' `xor` v3'
    word = BS.foldr' (\b acc -> (acc `shiftL` 8) .|. fromIntegral b) 0
    sipRound (v0, v1, v2, v3) =
      let a0 = v0 + v1
          a1 = rotateL v1 13 `xor` a0
          a0' = rotateL a0 32
          a2 = v2 + v3
          a3 = rotateL v3 16 `xor` a2
          b0 = a0' + a3
          b3 = rotateL a3 21 `xor` b0
          b2 = a2 + a1
          b1 = rotateL a1 17 `xor` b2
          b2' = rotateL b2 32
      in  (b0, b1, b2', b3)
