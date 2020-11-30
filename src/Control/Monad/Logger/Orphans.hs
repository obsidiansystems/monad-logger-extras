{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Description: Orphan instances for monad-logger's LoggingT
|-}
module Control.Monad.Logger.Orphans where

import Control.Monad.Logger

import Control.Applicative (Alternative(..))
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans (lift)

instance (Monad m, Alternative m) => Alternative (LoggingT m) where
  empty = lift empty
  a <|> b = LoggingT $ \r -> let LoggingT f' = a <|> b in f' r

instance (Monad m, Alternative m) => MonadPlus (LoggingT m)
