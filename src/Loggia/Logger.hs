{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Loggia.Logger where

import BasicPrelude hiding (for, log)
import Pipes

--------------------------------------------------------------------------------

data Log = Info Text | Warning Text | Error Text | Debug Text deriving (Show, Eq, Ord)

newtype LoggerT l m a = LoggerT
  { _runLoggerT :: Producer l m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

info, warn, throw, debug :: (Monad m) => Text -> LoggerT Log m ()
info = LoggerT . yield . Info
warn = LoggerT . yield . Warning
throw = LoggerT . yield . Error
debug = LoggerT . yield . Debug

runLoggerT :: (MonadIO m) => (l -> IO ()) -> LoggerT l m a -> m a
runLoggerT f (LoggerT l) = runEffect $ for l (liftIO . f)

runStdOutLogger :: (Show l, MonadIO m) => LoggerT l m a -> m a
runStdOutLogger = runLoggerT print
