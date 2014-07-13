{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Gossim.Internal.Logging
       ( MonadLogger(monadLoggerLog)
       , LoggingT
       , Loc
       , LogSource
       , LogLevel(LevelDebug, LevelInfo, LevelWarn, LevelError)
       , LogStr
       , ToLogStr(toLogStr)
       , Format
       , Only(Only)
       , Params
       , format
       , logDebug
       , logInfo
       , logWarn
       , logError
       , logGeneric
       , runStdoutLoggingT
       ) where


import Language.Haskell.TH.Syntax (Lift (lift), Q, Exp, qLocation)

import Control.Monad.Logger (MonadLogger(monadLoggerLog),
                             LoggingT,
                             Loc, LogSource,
                             LogLevel(LevelDebug, LevelInfo,
                                      LevelWarn, LevelError),
                             liftLoc, runStdoutLoggingT)
import System.Log.FastLogger (LogStr, ToLogStr(toLogStr))

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Format (Format, Only(Only))
import qualified Data.Text.Format as Fmt
import Data.Text.Format.Params (Params)


------------------------------------------------------------------------------
format :: Params ps => Format -> ps -> Text
format fmt = toStrict . Fmt.format fmt


------------------------------------------------------------------------------
logDebug :: Q Exp
logDebug = logTH LevelDebug

logInfo :: Q Exp
logInfo = logTH LevelInfo

logWarn :: Q Exp
logWarn = logTH LevelWarn

logError :: Q Exp
logError = logTH LevelError

logGeneric :: LogLevel -> Q Exp
logGeneric = logTH

logTH :: LogLevel -> Q Exp
logTH level =
    [|\fmt ->
         monadLoggerLog $(qLocation >>= liftLoc) "" $(lift level) . format fmt|]
