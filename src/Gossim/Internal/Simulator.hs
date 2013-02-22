{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

import Prelude hiding (log)

import Control.Lens (makeLenses, use, (+=))
import Control.Monad.Trans (MonadIO)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.State.Strict (StateT, MonadState)

import Data.IntMap.Strict (IntMap)

import System.Log.Simple (MonadLog(askLog), Log)

import Gossim.Internal.Agent (Agent)
import Gossim.Internal.Types (Time,
                              AgentId,
                              Rumor(Rumor), RumorId(RumorId))
import Gossim.Internal.Random (RandomT, MonadRandom, randomRInt, randomMaybeM)


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
  GossimConfig { duration  :: Time
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
defaultNewRumorRF :: RandomFunction (Maybe Rumor)
defaultNewRumorRF =
  independent $ randomMaybeM 0.4 $ do
    size <- randomRInt (1, 100)
    createRumor size

defaultAgentFailureRF :: RandomFunction Bool
defaultAgentFailureRF = independent $ return False

defaultConfig :: GossimConfig
defaultConfig =
  GossimConfig { duration  = 1000
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
simulate = undefined
