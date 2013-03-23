{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

import Prelude hiding (mapM, mapM_)

import Control.Applicative ((<$>))
import Control.Arrow ((&&&))
import Control.Lens (makeLenses, use, (%=), (<<%=), (<<.=), (.=))
import Control.Monad (replicateM, when)
import Control.Monad.Trans (MonadIO)
import Control.Monad.CatchIO (MonadCatchIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT)

import Data.Foldable (mapM_)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)

import Data.Dynamic (Dynamic)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap

import Gossim.Internal.Agent (Agent, AgentState, AgentEnv(AgentEnv),
                              Action(Log, Send, Receive, Discovered),
                              newAgentState, bounce)
import qualified Gossim.Internal.Agent as Agent

import Gossim.Internal.Types (Time,
                              AgentId(AgentId),
                              Rumor(Rumor), RumorId(RumorId),
                              rumorId, unRumorId)
import Gossim.Internal.Random (RandomT, MonadRandom, Seed,
                               evalRandomT, newSeed,
                               randomRInt, randomMaybeM)
import Gossim.Internal.Logging (Log, Level(Trace),
                                MonadLog(askLog), Only(Only),
                                initLogging,
                                logM, debugM, infoM, scope, format)


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
              , _agents      :: IntMap (Agent (), AgentState)
              , _messageQueues  :: IntMap (Seq Dynamic)
              -- TODO: seems that it doesn't need to be a Seq; list will do as
              -- well
              , _runnableAgents :: Seq Int

              , _nextRumorId :: Int
              , _rumors      :: IntMap Rumor
              }

makeLenses ''GossimState


instance MonadLog Gossim where
  askLog = use logger


------------------------------------------------------------------------------
runGossim :: Gossim a -> GossimConfig -> GossimState -> Seed -> IO a
runGossim (Gossim a) config state =
  evalRandomT (evalStateT (runReaderT a config) state)


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

rumorIdInt :: Rumor -> Int
rumorIdInt = unRumorId . rumorId


getNextAgentId :: GossimPure m => m Int
getNextAgentId = nextAgentId <<%= (+1)

tick :: GossimPure m => m Time
tick = time <<%= (+1)

------------------------------------------------------------------------------
simulate :: Agent () -> Text -> GossimConfig -> IO ()
simulate agent title config@(GossimConfig {logLevel}) = do
  logger <- initLogging logLevel
  seed <- newSeed
  let initialState = GossimState { _logger         = logger
                                 , _time           = 0
                                 , _nextAgentId    = 0
                                 , _agents         = IntMap.empty
                                 , _runnableAgents = Seq.empty
                                 , _messageQueues  = IntMap.empty
                                 , _nextRumorId    = 0
                                 , _rumors         = IntMap.empty
                                 }

  runGossim (scope "simulator" $ doSimulate agent title) config initialState seed

doSimulate :: Agent () -> Text -> Gossim ()
doSimulate agent title = do
  infoM "Starting {} simulation" (Only title)
  numAgents <- asks numAgents
  agentIds  <- replicateM numAgents getNextAgentId
  agentStates <- replicateM numAgents newAgentState
  agents .= IntMap.fromList [(aid, (agent, astate)) | aid <- agentIds
                                                    | astate <- agentStates]
  runnableAgents .= Seq.fromList agentIds
  messageQueues .= IntMap.fromList [(aid, Seq.empty) | aid <- agentIds]
  infoM "Created {} agents" (Only numAgents)

  -- TODO: unfix this
  rs <- replicateM 1000 (createRumor 100)
  rumors .= IntMap.fromList (map (rumorIdInt &&& id) rs)

  step agent

step :: Agent () -> Gossim ()
step agent = do
  time <- tick
  done <- (time >=) <$> asks duration

  if done
    then infoM "Finished simulation after {} steps" (Only time)
    else doStep agent

doStep :: Agent () -> Gossim ()
doStep _ = do
  envRumors <- use rumors
  envAgents <- map AgentId <$> IntMap.keys <$> use agents
  let envTemplate = AgentEnv { Agent.self = error "Use of uninitialized self"
                             , Agent.rumors = envRumors
                             , Agent.agents = envAgents
                             }

  -- runnableAgents gets updated for us by processRunnable; so we just have to
  -- empty it before
  runnable <- (runnableAgents <<.= Seq.empty)
  mapM_ (processRunnable envTemplate) runnable

  debugM "Processed all runnable agents" ()

processRunnable :: AgentEnv -> Int -> Gossim ()
processRunnable envTemplate aid = do
  debugM "Processing runnable agent {}" (Only aid)
  (agent, astate) <- getAgent aid
  processAgent env aid agent astate

  where env = envTemplate { Agent.self = AgentId aid }

processAgent :: AgentEnv -> Int -> Agent () -> AgentState -> Gossim ()
processAgent env aid agent astate =
  case cont of
    Right () -> do
      infoM "Agent {} terminated" (Only aid)
      agents %= IntMap.delete aid
      messageQueues %= IntMap.delete aid
    Left action -> do
      (agent', agentRunnable) <- processAction aid action
      when agentRunnable (runnableAgents %= (|> aid))
      agents %= IntMap.insert aid (agent', astate')
  where (astate', cont) = bounce agent env astate

processAction :: Int -> Action (Agent ()) -> Gossim (Agent (), Bool)
processAction aid (Log level text s) = do
  scope (format "{}" (Only $ AgentId aid)) $
    logM level "{}" (Only text)
  return (s, True)
processAction _ (Send (AgentId dst) msg s) = do
  messageQueues %= IntMap.update (Just . (|> msg)) dst
  return (s, True)
processAction aid (Receive _ c) = do
  debugM "Ignoring receive from {}" (Only $ AgentId aid)
  return (c undefined, undefined)
processAction _ (Discovered _ s) =
  -- TODO
  return (s, True)

getAgent :: Int -> Gossim (Agent (), AgentState)
getAgent aid = extract <$> IntMap.lookup aid <$> use agents
  where extract = fromMaybe (error $ "Cannot find agent " ++ show aid)
