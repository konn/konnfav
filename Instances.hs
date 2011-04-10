{-# OPTIONS_GHC -fwarn-unused-imports #-}
module Instances () where
import Control.Monad.IO.Control
import Data.Enumerator
import Control.Monad.IO.Class
import Control.Exception.Control

instance MonadIO m => MonadControlIO (Iteratee a m) where
  liftControlIO f = liftIO $ f run'
    where
      run' iter = return $ Iteratee $ do
        stp <- runIteratee iter
        case stp of
          Error exc -> throwIO exc
          s         -> return s
