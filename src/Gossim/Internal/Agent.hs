{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gossim.Internal.Agent
       ( ReceiveHandler(Handler)
       , Action (Log, Broadcast, Receive, Discovered)
       , AgentEnv (AgentEnv, self, master, agents, rumors)
       , Agent (Agent, unAgent)
       , AgentState
       , agentState
       , newAgentState
       , bounce
       , broadcast
       , send
       , (!)
       , receive
       , receiveMany
       , discovered
       , getAgents
       , getSelf
       , getRumor
       , getMaster
       , isMaster
       ) where

import Control.Applicative (Applicative, (<$>), (<*>))
import Control.Monad (liftM, join)
import Control.Monad.Trans (MonadTrans(lift), MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader(ask, local, reader),
                             runReaderT, asks)
import Control.Monad.Coroutine (Coroutine(resume), mapMonad, suspend)

import Data.Maybe (fromMaybe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)

import Data.Dynamic (Dynamic, toDyn)
import Data.Typeable (Typeable)

import Gossim.Internal.Random (Random, Seed, MonadRandom(liftRandom),
                               runRandom, newSeed)
import Gossim.Internal.Types (AgentId, Rumor, RumorId(RumorId))
import Gossim.Internal.Logging (MonadLogPure(doLog), Level)

data ReceiveHandler r where
  Handler :: Typeable a => (a -> Agent r) -> ReceiveHandler r

data Action s where
  Log :: Level -> Text -> s -> Action s
  Broadcast :: [AgentId] -> Dynamic -> s -> Action s
  Discovered :: Rumor -> s -> Action s
  Receive :: [ReceiveHandler a] -> (Agent a -> s) -> Action s

instance Functor Action where
  fmap f (Log level text s) = Log level text (f s)
  fmap f (Broadcast dst msg s) = Broadcast dst msg (f s)
  fmap f (Discovered r s) = Discovered r (f s)
  fmap f (Receive handlers s) = Receive handlers (f . s)

data AgentEnv = AgentEnv { self   :: AgentId
                         , master :: AgentId
                         , agents :: [AgentId]
                         , rumors :: IntMap Rumor
                         }

newtype Agent a =
  Agent { unAgent :: Coroutine Action (ReaderT AgentEnv Random) a }
  deriving (Monad, Functor, Applicative)

newtype AgentState = AgentState Seed

instance MonadReader AgentEnv Agent where
  ask     = Agent $ lift ask
  reader  = Agent . lift . reader
  local f = Agent . mapMonad (local f) . unAgent

instance MonadRandom Agent where
  liftRandom = Agent . liftRandom

instance MonadLogPure Agent where
  doLog level text = Agent $ suspend (Log level text (return ()))


------------------------------------------------------------------------------
agentState :: Seed -> AgentState
agentState = AgentState

newAgentState :: MonadIO m => m AgentState
newAgentState = liftM AgentState newSeed

bounce :: Agent a -> AgentEnv -> AgentState
       -> (AgentState, Either (Action (Agent a)) a)
bounce (Agent c) env (AgentState seed) = (state, left (fmap Agent) c')
  where (c', seed') = runRandom (runReaderT (resume c) env) seed
        state = AgentState seed'

        left f (Left x)  = Left (f x)
        left _ (Right x) = Right x

------------------------------------------------------------------------------
broadcast :: Typeable msg => [AgentId] -> msg -> Agent ()
broadcast dsts msg = Agent $ suspend (Broadcast dsts (toDyn msg) (return ()))

send :: Typeable msg => AgentId -> msg -> Agent ()
send dst = broadcast [dst]

(!) :: Typeable msg => AgentId -> msg -> Agent ()
(!) = send

receive :: Typeable a => (a -> Agent r) -> Agent r
receive h = receiveMany [Handler h]

receiveMany :: [ReceiveHandler a] -> Agent a
receiveMany handlers = join $ Agent $ suspend (Receive handlers return)

discovered :: Rumor -> Agent ()
discovered rumor = Agent $ suspend (Discovered rumor (return ()))

getAgents :: Agent [AgentId]
getAgents = asks agents

getSelf :: Agent AgentId
getSelf = asks self

getRumor :: RumorId -> Agent Rumor
getRumor (RumorId rid) =
  fromMaybe reportError . IntMap.lookup rid <$> asks rumors
  where reportError = error $ "Impossible: no rumor with id " ++ show rid

getMaster :: Agent AgentId
getMaster = asks master

isMaster :: Agent Bool
isMaster = (==) <$> getSelf <*> getMaster
