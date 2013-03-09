{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gossim.Internal.Agent
       ( ReceiveHandler(Handler)
       , Action (Log, Send, Receive, Discovered)
       , AgentEnv (AgentEnv, self, agents, rumors)
       , Agent (Agent, unAgent)
       , AgentState
       , agentState
       , newAgentState
       , bounce
       , send
       , (!)
       , receive
       , discovered
       , getAgents
       , getSelf
       , getRumor
       ) where

import Control.Applicative ((<$>))
import Control.Monad (join, liftM)
import Control.Monad.Trans (MonadTrans(lift), MonadIO)
import Control.Monad.Reader (ReaderT, MonadReader(ask, local, reader),
                             runReaderT, asks)
import Control.Monad.Coroutine (Coroutine(resume), mapMonad, suspend)

import Data.Maybe (fromMaybe)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import Data.Typeable (Typeable)

import Gossim.Internal.Random (Random, Seed, MonadRandom(liftRandom),
                               runRandom, newSeed)
import Gossim.Internal.Types (AgentId, Rumor, RumorId(RumorId))
import Gossim.Internal.Logging (MonadLogPure(doLog), Level)

data ReceiveHandler s where
  Handler :: Typeable a => (a -> s) -> ReceiveHandler s

instance Functor ReceiveHandler where
  fmap f (Handler h) = Handler $ fmap f h

data Action s where
  Log :: Level -> Text -> s -> Action s
  Send :: Typeable msg => AgentId -> msg -> s -> Action s
  Discovered :: Rumor -> s -> Action s
  Receive :: [ReceiveHandler a] -> (a -> s) -> Action s

instance Functor Action where
  fmap f (Log level text s) = Log level text (f s)
  fmap f (Send dst msg s) = Send dst msg (f s)
  fmap f (Discovered r s) = Discovered r (f s)
  fmap f (Receive handlers s) = Receive handlers (f . s)

data AgentEnv = AgentEnv { self   :: AgentId
                         , agents :: [AgentId]
                         , rumors :: IntMap Rumor
                         }

newtype Agent a =
  Agent { unAgent :: Coroutine Action (ReaderT AgentEnv Random) a }
  deriving (Monad, Functor)

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
send :: Typeable msg => AgentId -> msg -> Agent ()
send dst msg = Agent $ suspend (Send dst msg (return ()))

(!) :: Typeable msg => AgentId -> msg -> Agent ()
(!) = send

receive :: [ReceiveHandler (Agent a)] -> Agent a
receive handlers = join $ Agent $ suspend (Receive handlers return)

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
