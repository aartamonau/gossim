{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (log)

import Control.Lens (makeLenses, use, (+=))
import Control.Monad.Trans (MonadIO)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT)

import Data.IntMap.Strict (IntMap)

import System.Log.Simple (MonadLog(askLog), Log, Level(Trace, Debug, Fatal),
                          Politics(Politics, politicsLow, politicsHigh),
                          rule, root, newLog, constant,
                          logger, text, console)
import qualified System.Log.Simple as SimpleLog

import Gossim.Internal.Agent (Agent)
import Gossim.Internal.Types (Time,
                              AgentId,
                              Rumor(Rumor), RumorId(RumorId))
import Gossim.Internal.Random (RandomT, MonadRandom, Seed,
                               runRandomT, newSeed,
                               randomRInt, randomMaybeM)


------------------------------------------------------------------------------
newtype Gossim a =
  Gossim (ReaderT GossimConfig (StateT GossimState (RandomT IO)) a)
  deriving (Monad, MonadRandom,
            MonadState GossimState, MonadReader GossimConfig,
            MonadIO, MonadCatchIO)

type GossimPure m = (Monad m, MonadRandom m,
                     MonadState GossimState m, MonadReader GossimConfig m)

type RandomFunction a = GossimPure m => Time -> AgentId -> m a

data GossimConfig =
  GossimConfig { logLevel :: Level

               , duration  :: Time
               , numAgents :: Int

               , newRumorRF     :: RandomFunction (Maybe Rumor)
               , agentFailureRF :: RandomFunction Bool
               }

data GossimState =
  GossimState { _log :: Log

              , _nextAgendId :: Int
              , _agents      :: IntMap (Agent ())

              , _nextRumorId :: Int
              , _rumors      :: IntMap Rumor
              }

makeLenses ''GossimState


instance MonadLog Gossim where
  askLog = use log


------------------------------------------------------------------------------
runGossim :: Gossim a -> GossimConfig -> GossimState -> Seed -> IO a
runGossim (Gossim a) config state seed =
  runRandomT (evalStateT (runReaderT a config) state) seed


------------------------------------------------------------------------------
defaultNewRumorRF :: RandomFunction (Maybe Rumor)
defaultNewRumorRF =
  independent $ randomMaybeM 0.4 $ do
    size <- randomRInt (1, 100)
    createRumor size

defaultAgentFailureRF :: RandomFunction Bool
defaultAgentFailureRF = independent $ return False

defaultConfig :: GossimConfig
defaultConfig =
  GossimConfig { logLevel = Trace

               , duration  = 1000
               , numAgents = 50

               , newRumorRF     = defaultNewRumorRF
               , agentFailureRF = defaultAgentFailureRF
               }


------------------------------------------------------------------------------
stationary :: a -> (Time -> a)
stationary = const

forallAgents :: a -> (AgentId -> a)
forallAgents = const

independent :: a -> (Time -> AgentId -> a)
independent = stationary . forallAgents

getNextRumorId :: GossimPure m => m RumorId
getNextRumorId = do
  rid <- use nextRumorId
  nextRumorId += 1

  return $ RumorId rid

createRumor :: GossimPure m => Int -> m Rumor
createRumor size = do
  rid <- getNextRumorId
  return $ Rumor rid size


------------------------------------------------------------------------------
simulate :: Agent () -> GossimConfig -> IO ()
simulate agent config@(GossimConfig {logLevel}) = do
  log  <- newLog (constant logRules) [logger text console]
  seed <- newSeed
  let initialState = GossimState { _log = log }

  runGossim doSimulate config initialState seed

  where logPolitics = Politics { politicsLow  = logLevel
                               , politicsHigh = Fatal
                               }
        logRules    = [rule root $ SimpleLog.use logPolitics]


doSimulate :: Gossim ()
doSimulate = SimpleLog.log Debug "test\n"
