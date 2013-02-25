module Gossim.Internal.Logging
       ( Log
       , Level(Trace, Debug, Info, Warning, Error, Fatal)
       , MonadLog(askLog)
       , initLogging
       , logM
       , traceM
       , debugM
       , infoM
       , warningM
       , errorM
       , fatalM
       , scope
       ) where

import Prelude hiding (log)

import Data.Text.Lazy (toStrict)
import Data.Text.Format (Format, format)
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
logM :: (Params ps, MonadLog m) => Level -> Format -> ps -> m ()
logM level fmt = log level . toStrict . format fmt

traceM :: (Params ps, MonadLog m) => Format -> ps -> m ()
traceM = logM Trace

debugM :: (Params ps, MonadLog m) => Format -> ps -> m ()
debugM = logM Debug

infoM :: (Params ps, MonadLog m) => Format -> ps -> m ()
infoM = logM Info

warningM :: (Params ps, MonadLog m) => Format -> ps -> m ()
warningM = logM Warning

errorM :: (Params ps, MonadLog m) => Format -> ps -> m ()
errorM = logM Error

fatalM :: (Params ps, MonadLog m) => Format -> ps -> m ()
fatalM = logM Fatal
