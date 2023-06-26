{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description: Orphan instances for monad-logger's LoggingT
-}
module Control.Monad.Logger.Orphans where

import Control.Monad.Logger

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))

instance (Monad m, Alternative m) => MonadPlus (LoggingT m)
