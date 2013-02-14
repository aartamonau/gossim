{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Gossip.DSL where

import Control.Lens ((%=), makeLenses)

import Control.Monad.Reader (Reader)
import Control.Monad.State.Strict (StateT, MonadState)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IntMap

import Data.Typeable (Typeable)

newtype Agent = Agent Int
              deriving Eq

newtype RumorId = RumorId Int
data Rumor = Rumor RumorId
           deriving Typeable

rumorId :: Rumor -> RumorId
rumorId (Rumor rid) = rid

getRumor :: RumorId -> GossipM Rumor
getRumor = undefined

data ReceiveHandler b where
  Handler :: Typeable a => (a -> GossipM b) -> ReceiveHandler b

data Action where
  Send :: Typeable msg => Agent -> msg -> GossipM r -> Action
  Receive :: [ReceiveHandler a] -> GossipM r -> Action
  Discovered :: Rumor -> GossipM r -> Action

data Message where
  Msg :: Typeable msg => msg -> Message

data GossipEnv = GEnv { agents :: [Agent]
                      , rumors :: IntMap Rumor
                      }

data GossipState = GState { _messages :: [(Agent, Message)]
                          , _actions :: [Action]
                          }
newtype GossipM a = GossipM (StateT GossipState (Reader GossipEnv) a)
                  deriving (Monad, MonadState GossipState)

makeLenses ''GossipState

send :: Typeable msg => Agent -> msg -> GossipM ()
send dst msg =
  messages %= ((dst, Msg msg) :)

(!) :: Typeable msg => Agent -> msg -> GossipM ()
(!) = send

receive :: [ReceiveHandler a] -> GossipM a
receive = undefined

discovered :: Rumor -> GossipM ()
discovered = undefined

getNodes :: GossipM [Agent]
getNodes = undefined
