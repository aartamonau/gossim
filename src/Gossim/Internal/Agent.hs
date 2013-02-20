{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gossim.Internal.Agent
       ( ReceiveHandler(Handler)
       , Action (Send, Receive, Discovered)
       , AgentEnv (AgentEnv, self, agents, rumors)
       , Agent (Agent, unAgent)
       ) where

import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Reader (ReaderT, MonadReader(ask, local, reader))
import Control.Monad.State (State, MonadState(get, put, state))
import Control.Monad.Coroutine (Coroutine, mapMonad)

import Data.IntMap.Strict (IntMap)
import Data.Typeable (Typeable)

import System.Random.Mersenne.Pure64 (PureMT)

import Gossim.Internal.Types (AgentId, Rumor)

data ReceiveHandler s where
  Handler :: Typeable a => (a -> s) -> ReceiveHandler s

instance Functor ReceiveHandler where
  fmap f (Handler h) = Handler $ fmap f h

data Action s where
  Send :: Typeable msg => AgentId -> msg -> s -> Action s
  Discovered :: Rumor -> s -> Action s
  Receive :: [ReceiveHandler a] -> (a -> s) -> Action s

instance Functor Action where
  fmap f (Send dst msg s) = Send dst msg (f s)
  fmap f (Discovered r s) = Discovered r (f s)
  fmap f (Receive handlers s) = Receive handlers (f . s)

data AgentEnv = AgentEnv { self   :: AgentId
                         , agents :: [AgentId]
                         , rumors :: IntMap Rumor
                         }

newtype Agent a =
  Agent { unAgent :: Coroutine Action (ReaderT AgentEnv (State PureMT)) a }
  deriving (Monad, Functor)

instance MonadReader AgentEnv Agent where
  ask     = Agent $ lift ask
  reader  = Agent . lift . reader
  local f = Agent . mapMonad (local f) . unAgent

instance MonadState PureMT Agent where
  get = Agent $ lift get
  put = Agent . lift . put
  state = Agent . lift . state
