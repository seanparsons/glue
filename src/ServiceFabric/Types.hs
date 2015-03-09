module ServiceFabric.Types where

import Control.Concurrent
import Control.Exception.Base hiding(throw)
import Control.Monad.CatchIO
import Control.Monad.IO.Class

type BasicService m a b = a -> m b
type ResultVar a = MVar (Either SomeException a)

-- Utils?
getResult :: (MonadCatchIO m) => ResultVar a -> m a
getResult var = do
  result <- liftIO $ readMVar var
  either throw return result