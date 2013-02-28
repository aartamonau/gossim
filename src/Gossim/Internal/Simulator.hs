{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>))
import Control.Lens (makeLenses, use, (<<%=))
import Control.Monad.Trans (MonadIO)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap

import Gossim.Internal.Agent (Agent)
import Gossim.Internal.Types (Time,
                              AgentId,
                              Rumor(Rumor), RumorId(RumorId))
import Gossim.Internal.Random (RandomT, MonadRandom, Seed,
                               runRandomT, newSeed,
                               randomRInt, randomMaybeM)
import Gossim.Internal.Logging (Log, Level(Trace), MonadLog(askLog),
                                initLogging, debugM, scope)


------------------------------------------------------------------------------
newtype Gossim a =
  Gossim (ReaderT GossimConfig (StateT GossimState (RandomT IO)) a)
  deriving (Functor, Monad, MonadRandom,
            MonadState GossimState, MonadReader GossimConfig,
            MonadIO, MonadCatchIO)

type GossimPure m = (Functor m, Monad m, MonadRandom m,
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
  GossimState { _logger :: Log

              , _time :: Time

              , _nextAgentId :: Int
              , _agents      :: IntMap (Agent ())

              , _nextRumorId :: Int
              , _rumors      :: IntMap Rumor
              }

makeLenses ''GossimState


instance MonadLog Gossim where
  askLog = use logger


------------------------------------------------------------------------------
runGossim :: Gossim a -> GossimConfig -> GossimState -> Seed -> IO a
runGossim (Gossim a) config state =
  runRandomT (evalStateT (runReaderT a config) state)


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
stationary :: a -> Time -> a
stationary = const

forallAgents :: a -> AgentId -> a
forallAgents = const

independent :: a -> Time -> AgentId -> a
independent = stationary . forallAgents


getNextRumorId :: GossimPure m => m RumorId
getNextRumorId = RumorId <$> (nextRumorId <<%= (+1))

createRumor :: GossimPure m => Int -> m Rumor
createRumor size = do
  rid <- getNextRumorId
  return $ Rumor rid size


getNextAgentId :: GossimPure m => m Int
getNextAgentId = nextAgentId <<%= (+1)


------------------------------------------------------------------------------
simulate :: Agent () -> GossimConfig -> IO ()
simulate agent config@(GossimConfig {logLevel}) = do
  logger <- initLogging logLevel
  seed <- newSeed
  let initialState = GossimState { _logger      = logger
                                 , _time        = 0
                                 , _nextAgentId = 0
                                 , _agents      = IntMap.empty
                                 , _nextRumorId = 0
                                 , _rumors      = IntMap.empty
                                 }

  runGossim (scope "simulator" $ doSimulate agent) config initialState seed

doSimulate :: Agent () -> Gossim ()
doSimulate _ = debugM "test" ()
