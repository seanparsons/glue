module ServiceFabric.Types(
    BasicService
  , MultiGetService
  , ResultVar
  , multiGetToBasic
  , basicToMultiGet
  , getResult
) where

import Control.Applicative
import Data.Hashable
import Control.Concurrent
import Control.Exception.Base hiding(throw)
import Control.Monad.CatchIO
import Control.Monad.IO.Class
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M

type BasicService m a b = a -> m b
type MultiGetService m a b = (S.HashSet a) -> m (M.HashMap a b)
type ResultVar a = MVar (Either SomeException a)

multiGetToBasic :: (Hashable a, Eq a, Monad m) => MultiGetService m a b -> BasicService m a b
multiGetToBasic service = (\r -> do
  mapResult <- service (S.singleton r)
  let possibleResult = M.lookup r mapResult
  maybe (fail "Invalid result.") return possibleResult)

basicToMultiGet :: (Hashable a, Eq a, Applicative m) => BasicService m a b -> MultiGetService m a b
basicToMultiGet service = 
  let callService resultMap request    = liftA2 (flip $ M.insert request) resultMap (service request)
  in  S.foldl' callService (pure M.empty)

-- Utils?
getResult :: (MonadCatchIO m) => ResultVar a -> m a
getResult var = do
  result <- liftIO $ readMVar var
  either throw return result