{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Gossim.Internal.Logging
       ( Log
       , Level(Trace, Debug, Info, Warning, Error, Fatal)
       , MonadLog(askLog)
       , MonadLogPure(doLog)
       , Only(Only)
       , initLogging
       , logM
       , traceM
       , debugM
       , infoM
       , warningM
       , errorM
       , fatalM
       , scope
       , format
       ) where

import Prelude hiding (log)

import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Format (Format, Only(Only))
import qualified Data.Text.Format as Fmt
import Data.Text.Format.Params (Params)

import System.Log.Simple (MonadLog(askLog), Log,
                          Level(Trace, Debug, Info, Warning, Error, Fatal),
                          Politics(Politics, politicsLow, politicsHigh),
                          rule, root, newLog, constant,
                          logger, text, console, log, use, scope)


------------------------------------------------------------------------------
initLogging :: Level -> IO Log
initLogging level = newLog (constant logRules) [logger text console]
  where logRules = [rule root $ use logPolitics]
        logPolitics = Politics { politicsLow  = level
                               , politicsHigh = Fatal
                               }


------------------------------------------------------------------------------
class Monad m => MonadLogPure m where
  doLog :: Level -> Text -> m ()

instance MonadLog m => MonadLogPure m where
  doLog = log


------------------------------------------------------------------------------
format :: Params ps => Format -> ps -> Text
format fmt = toStrict . Fmt.format fmt


------------------------------------------------------------------------------
logM :: (Params ps, MonadLogPure m) => Level -> Format -> ps -> m ()
logM level fmt = doLog level . format fmt

traceM :: (Params ps, MonadLogPure m) => Format -> ps -> m ()
traceM = logM Trace

debugM :: (Params ps, MonadLogPure m) => Format -> ps -> m ()
debugM = logM Debug

infoM :: (Params ps, MonadLogPure m) => Format -> ps -> m ()
infoM = logM Info

warningM :: (Params ps, MonadLogPure m) => Format -> ps -> m ()
warningM = logM Warning

errorM :: (Params ps, MonadLogPure m) => Format -> ps -> m ()
errorM = logM Error

fatalM :: (Params ps, MonadLogPure m) => Format -> ps -> m ()
fatalM = logM Fatal
