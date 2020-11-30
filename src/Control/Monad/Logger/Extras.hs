{-|
Description: Composable logging actions for monad-logger.
|-}
module Control.Monad.Logger.Extras where

import Control.Monad.Logger

import Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import System.IO
import qualified System.Posix.Syslog as Posix

-- | Run a 'LoggingT' action using the provided 'Logger'
runLoggerLoggingT :: LoggingT m a -> Logger -> m a
runLoggerLoggingT f logger = f `runLoggingT` unLogger logger

-- | Type synonym for a logging action. See 'defaultLogStr' for the default
-- formatting of this data.
type LogF = Loc -> LogSource -> LogLevel -> LogStr -> IO ()

-- | A composable logging action. Logging actions will be run one after another
-- in the order they are composed.
newtype Logger = Logger { unLogger :: LogF }

instance Semigroup Logger where
  Logger a <> Logger b = Logger $ \loc src lvl str -> a loc src lvl str >> b loc src lvl str

instance Monoid Logger where
  mappend = (<>)
  mempty = Logger $ \_ _ _ _ -> mempty

-- | Composable stderr logging action.
logToStderr :: Logger
logToStderr = Logger $ defaultOutput stderr

-- | Composable stdout logging action.
logToStdout :: Logger
logToStdout = Logger $ defaultOutput stdout

-- | This logger doesn't perform any logging action.
logToNowhere :: Logger
logToNowhere = mempty

-- | Log messages to a posix system log.
logToSyslog :: String -> Logger
logToSyslog tagstr = Logger $ \loc src lvl str -> do
  let syslogPriority = case lvl of
        LevelDebug -> Posix.Debug
        LevelInfo -> Posix.Info
        LevelWarn -> Posix.Warning
        LevelError -> Posix.Error
        LevelOther _ -> Posix.Info
      out = defaultLogStr loc src lvl str
  Posix.withSyslog tagstr [Posix.DelayedOpen] Posix.User $
    unsafeUseAsCStringLen (fromLogStr out) $
      Posix.syslog Nothing syslogPriority
