{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Gossim.Internal.Simulator
       where

import Prelude hiding (mapM, mapM_)

import Control.Applicative (Applicative, (<$>))
import Control.Lens (makeLenses, use, uses, mapMOf_, folded, _2,
                     (%=), (<<%=), (<<.=), (.=))
import Control.Monad (replicateM, liftM, forM_)
import Control.Monad.Trans (MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, asks)
import Control.Monad.State.Strict (StateT, MonadState, evalStateT)

import Data.Default (Default(def))
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL(EmptyL, (:<)), (><), (|>))
import qualified Data.Sequence as Seq
import Data.Text (Text)

import Data.Dynamic (Dynamic, fromDynamic)
import Data.Typeable (cast)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap

import Data.IntSet (IntSet)
import Data.IntSet.Lens (members)
import qualified Data.IntSet as IntSet

import Data.PQueue.Prio.Min (MinPQueue)
import qualified Data.PQueue.Prio.Min as PQueue

import Gossim.Internal.Agent (Agent,
                              Action(Log, Broadcast, Receive, Random,
                                     GetSelf, GetAgents),
                              ReceiveHandler(Handler), bounce)

import Gossim.Internal.Types (Time, AgentId(AgentId), Tick)
import Gossim.Internal.Random (RandomT, MonadRandom(liftRandom), Seed,
                               evalRandomT, newSeed)
import Gossim.Internal.Logging (MonadLogger(monadLoggerLog),
                                LoggingT, Only(Only),
                                LogLevel(LevelDebug),
                                runStdoutLoggingT,
                                logDebug, logInfo)

import Gossim.Internal.Simulator.Scheduler (Scheduler)
import Gossim.Internal.Simulator.Scheduler as Sched

-- TODO: remove
import qualified Gossim.Protocol.PingPong as PP

rudimentary :: Agent ()
rudimentary = $(logDebug) "test" () >> rudimentary

------------------------------------------------------------------------------
newtype Gossim a =
  Gossim (ReaderT GossimConfig (StateT GossimState (RandomT (LoggingT IO))) a)
  deriving (Functor, Applicative, Monad, MonadRandom,
            MonadState GossimState, MonadReader GossimConfig,
            MonadIO, MonadLogger)

type GossimPure m = (Functor m, Monad m, MonadRandom m,
                     MonadState GossimState m, MonadReader GossimConfig m)

data RunnableState = Running | Runnable | Blocked

data GossimConfig =
  GossimConfig { logLevel :: LogLevel

               , duration  :: Time
               , numAgents :: Int
               }

type Nonce = Int
data SideEffect = Message Int Dynamic

data GossimState =
  GossimState { _time :: Time

                -- MinPQueue is not stable so this is used to preserve the
                -- order of side-effects
              , _sideEffectNonce :: Nonce
              , _pendingSideEffects :: MinPQueue (Time, Nonce) SideEffect

              , _nextAgentId :: Int
              , _agents      :: IntMap (Agent ())
              , _runnableStates :: IntMap RunnableState
              , _messageQueues  :: IntMap (Seq Dynamic)
              , _runnableAgents :: IntSet

              , _nextRumorId :: Int
              }

makeLenses ''GossimState


------------------------------------------------------------------------------
runGossim :: Gossim a -> GossimConfig -> GossimState -> Seed -> IO a
runGossim (Gossim a) config state seed =
  runStdoutLoggingT $ evalRandomT (evalStateT (runReaderT a config) state) seed


------------------------------------------------------------------------------
defaultConfig :: GossimConfig
defaultConfig =
  GossimConfig { logLevel = LevelDebug

               , duration  = 1000
               , numAgents = 50
               }

instance Default GossimConfig where
  def = defaultConfig

------------------------------------------------------------------------------
forallAgents :: a -> AgentId -> a
forallAgents = const

getNextAgentId :: GossimPure m => m Int
getNextAgentId = nextAgentId <<%= (+1)

tick :: GossimPure m => m Time
tick = time <<%= (+1)

getTime :: GossimPure m => m Time
getTime = use time

------------------------------------------------------------------------------
simulate :: Agent () -> Text -> GossimConfig -> IO ()
simulate agent title config@(GossimConfig {logLevel}) = do
  seed <- newSeed
  let initialState = GossimState { _time               = 0
                                 , _sideEffectNonce    = 0
                                 , _pendingSideEffects = PQueue.empty
                                 , _nextAgentId        = 0
                                 , _agents             = IntMap.empty
                                 , _runnableAgents     = IntSet.empty
                                 , _runnableStates     = IntMap.empty
                                 , _messageQueues      = IntMap.empty
                                 , _nextRumorId        = 0
                                 }

  runGossim (doSimulate agent title) config initialState seed

doSimulate :: Agent () -> Text -> Gossim ()
doSimulate agent title = do
  $logInfo "Starting {} simulation" (Only title)
  numAgents <- asks numAgents
  agentIds  <- replicateM numAgents getNextAgentId
  agents .= IntMap.fromList [(aid, agent) | aid <- agentIds]
  runnableAgents .= IntSet.fromList agentIds
  runnableStates .= IntMap.fromList [(aid, Runnable) | aid <- agentIds]
  messageQueues .= IntMap.fromList [(aid, Seq.empty) | aid <- agentIds]
  $logInfo "Created {} agents" (Only numAgents)

  step agent

step :: Agent () -> Gossim ()
step agent = do
  time <- tick
  resetNonce
  done <- (time >=) <$> asks duration

  if done
    then $logInfo "Finished simulation after {} steps" (Only time)
    else do
      $logDebug "Time {}" (Only time)
      processPendingSideEffects
      doStep agent
      step agent

processPendingSideEffects :: Gossim ()
processPendingSideEffects = do
  time <- getTime
  let pred (ts, _) _ = ts <= time
  (sideEffects, pending') <- PQueue.spanWithKey pred <$> use pendingSideEffects
  pendingSideEffects .= pending'
  $logDebug "Found {} pending side-effect(s)" (Only $ length sideEffects)
  mapMOf_ (folded._2) processSideEffect sideEffects

processSideEffect :: SideEffect -> Gossim ()
processSideEffect (Message dst msg) = do
  messageQueues %= IntMap.update (Just . (|> msg)) dst
  setRunnable dst

doStep :: Agent () -> Gossim ()
doStep _ = do
  -- runnableAgents gets updated for us by processRunnable; so we just have to
  -- empty it before
  runnable <- (runnableAgents <<.= IntSet.empty)
  $logDebug "Found {} runnable agent(s)" (Only $ IntSet.size runnable)
  mapMOf_ members processRunnable runnable

processRunnable :: Int -> Gossim ()
processRunnable aid = do
  $logDebug "Processing runnable agent {}" (Only aid)
  agent <- getAgent aid
  processAgent aid agent

processAgent :: Int -> Agent () -> Gossim ()
processAgent aid agent = do
  maybeNewAgent <- doProcessAgent aid agent
  case maybeNewAgent of
    Nothing -> do
      $logInfo "Agent {} terminated" (Only aid)
      agents %= IntMap.delete aid
      messageQueues %= IntMap.delete aid
      runnableStates %= IntMap.delete aid
    Just agent' ->
      agents %= IntMap.insert aid agent'

doProcessAgent :: Int -> Agent ()
               -> Gossim (Maybe (Agent ()))
doProcessAgent aid agent =
  case cont of
    Right () ->
      return Nothing
    Left action -> do
      setRunning aid
      maybeNewAgent <- processAction aid action
      case maybeNewAgent of
        Nothing -> return $ Just agent
        Just newAgent -> do
          proceed <- not <$> takesTick action
          if proceed
            then doProcessAgent aid newAgent
            else return $ Just newAgent
  where cont = bounce agent

actionCost :: Action a -> Tick
actionCost (Log _ _ _ _ _) = Sched.opCost 100
actionCost _ = Sched.opCost 10

takesTick :: Action (Agent ()) -> Gossim Bool
takesTick (Log _ _ _ _ _) = return False
takesTick _           = return True

processAction :: Int -> Action (Agent ()) -> Gossim (Maybe (Agent ()))
processAction aid (GetSelf c) = do
  setRunnable aid
  return $ Just (c (AgentId aid))
processAction aid (GetAgents c) = do
  setRunnable aid
  as <- map AgentId <$> IntMap.keys <$> use agents
  return $ Just (c as)
processAction aid (Random f c) = do
  r <- liftRandom f
  setRunnable aid
  return $ Just (c r)
processAction aid (Log loc source level text s) = do
  monadLoggerLog loc source level text
  setRunnable aid
  return $ Just s
processAction aid (Broadcast dsts msg s) = do
  $logDebug "Processing broadcast from agent {} to {} destination(s)" (aid, length dsts)
  forM_ dsts $ \(AgentId dst) ->
    queueSideEffect 0 (Message dst msg)
  setRunnable aid
  return $ Just s
processAction aid (Receive handlers c) = do
  messages <- getMessages aid
  $logDebug ("Processing receive (agent {}). \
              \We have {} message(s) in the queue") (aid, Seq.length messages)
  let maybeCont = findCont handlers messages
  case maybeCont of
    Nothing -> do
      setBlocked aid
      return Nothing
    Just (cont, messages') -> do
      $logDebug "Found matching receive handler" ()
      messageQueues %= IntMap.insert aid messages'
      setRunnable aid
      return $ Just (c cont)
    where findCont :: [ReceiveHandler r] -> Seq Dynamic
                   -> Maybe (Agent r, Seq Dynamic)
          findCont hs msgs = goMsgs msgs Seq.empty
            where goMsgs (Seq.viewl -> EmptyL) _ = Nothing
                  goMsgs (Seq.viewl -> msg :< rest) r =
                    case goHandlers msg hs of
                      Nothing -> goMsgs rest (r |> msg)
                      Just c  -> Just (c, r >< rest)
                  -- silence bogus non-exhaustive pattern warning
                  goMsgs _ _ = error "impossible"

                  goHandlers _ [] = Nothing
                  goHandlers msg (h : hs) =
                    case tryHandler h msg of
                      Nothing -> goHandlers msg hs
                      r       -> r

          tryHandler :: ReceiveHandler r -> Dynamic -> Maybe (Agent r)
          tryHandler (Handler (h :: a -> Agent r)) msg =
            liftM h (maybeMsg >>= cast)
            where maybeMsg :: Maybe a
                  maybeMsg = fromDynamic msg

getAgent :: Int -> Gossim (Agent ())
getAgent aid = extract <$> IntMap.lookup aid <$> use agents
  where extract = fromMaybe (error $ "Cannot find agent " ++ show aid)

getRunnableState :: Int -> Gossim RunnableState
getRunnableState aid = uses runnableStates extract
  where extract = fromMaybe reportError . IntMap.lookup aid
        reportError = error ("getRunnableState: missing agent " ++ show aid)

setRunnable :: Int -> Gossim ()
setRunnable aid = do
  runnableStates %= IntMap.insert aid Runnable
  runnableAgents %= IntSet.insert aid

setBlocked :: Int -> Gossim ()
setBlocked aid = do
  runnableStates %= IntMap.insert aid Blocked
  runnableAgents %= IntSet.delete aid

setRunning :: Int -> Gossim ()
setRunning aid = runnableStates %= IntMap.insert aid Running

getMessages :: Int -> Gossim (Seq Dynamic)
getMessages aid = uses messageQueues extract
  where extract = fromMaybe reportError . IntMap.lookup aid
        reportError = error ("getMessages: missing agent " ++ show aid)

resetNonce :: Gossim ()
resetNonce = sideEffectNonce .= 0

nextNonce :: Gossim Nonce
nextNonce = sideEffectNonce <<%= (+1)

queueSideEffect :: Time -> SideEffect -> Gossim ()
queueSideEffect delay effect
  | delay >= 0 = do
    expiryTime <- (+ delay) <$> getTime
    nonce <- nextNonce
    pendingSideEffects %= PQueue.insert (expiryTime, nonce) effect
  | otherwise = error $ "queueSideEffect: got invalid delay " ++ show delay
