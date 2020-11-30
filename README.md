monad-logger-extras
===================

Composable logging, syslog integration, and more with [monad-logger](https://hackage.haskell.org/package/monad-logger).

Description
-----------

This package provides a way to compose logging actions so that you can conveniently log to multiple destinations. It also includes implementations of a few common logging actions: logging to stdout, stderr, nowhere (similar to `NoLoggingT`), and to a posix syslog (using [hsyslog](https://hackage.haskell.org/package/hsyslog)).

It also contains a couple of orphan instances for [`LoggingT`](https://hackage.haskell.org/package/monad-logger-0.3.36/docs/Control-Monad-Logger.html#t:LoggingT): `MonadPlus` and `Alternative`.

Example usage
-------------

Watch for the system log message by running:

```bash
journalctl --user -t log-test -f
```

This example can be built and run using cabal (either `cabal repl example` or `cabal build example`).


```haskell

> {-# LANGUAGE OverloadedStrings #-}
> 
> import Control.Monad.Logger
> import Control.Monad.Logger.Extras
> 
> main :: IO ()
> main = do
>   let logger = logToStdout <> logToStderr <> logToSyslog "log-test"
>   flip runLoggerLoggingT logger $ do
>     logInfoN "This is a test. You should see this on stdout, stderr, and in your system log."

```
