{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Gossip.DSL where

import Control.Applicative ((<$>))

import Control.Monad (join)
import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Reader (Reader, MonadReader(ask, local, reader), asks)
import Control.Monad.Coroutine (Coroutine, mapMonad, suspend)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Data.Maybe (fromMaybe)

import Data.Typeable (Typeable)

newtype Agent = Agent Int
              deriving Eq

newtype RumorId = RumorId Int
                deriving Show

data Rumor = Rumor RumorId
           deriving Typeable

data ReceiveHandler s where
  Handler :: Typeable a => (a -> s) -> ReceiveHandler s

instance Functor ReceiveHandler where
  fmap f (Handler h) = Handler $ fmap f h

data Action s where
  Send :: Typeable msg => Agent -> msg -> s -> Action s
  Discovered :: Rumor -> s -> Action s
  Receive :: [ReceiveHandler a] -> (a -> s) -> Action s

instance Functor Action where
  fmap f (Send dst msg s) = Send dst msg (f s)
  fmap f (Discovered r s) = Discovered r (f s)
  fmap f (Receive handlers s) = Receive handlers (f . s)

data GossipEnv = GEnv { self   :: Agent
                      , agents :: [Agent]
                      , rumors :: IntMap Rumor
                      }

newtype GossipM a = GossipM { unGossipM :: Coroutine Action (Reader GossipEnv) a }
                  deriving (Monad, Functor)

instance MonadReader GossipEnv GossipM where
  ask     = GossipM $ lift ask
  reader  = GossipM . lift . reader
  local f = GossipM . mapMonad (local f) . unGossipM

send :: Typeable msg => Agent -> msg -> GossipM ()
send dst msg = GossipM $ suspend (Send dst msg (return ()))

(!) :: Typeable msg => Agent -> msg -> GossipM ()
(!) = send

receive :: [ReceiveHandler (GossipM a)] -> GossipM a
receive handlers = join $ GossipM $ suspend (Receive handlers return)

discovered :: Rumor -> GossipM ()
discovered rumor = GossipM $ suspend (Discovered rumor (return ()))

getAgents :: GossipM [Agent]
getAgents = asks agents

getSelf :: GossipM Agent
getSelf = asks self

rumorId :: Rumor -> RumorId
rumorId (Rumor rid) = rid

getRumor :: RumorId -> GossipM Rumor
getRumor (RumorId rid) =
  fromMaybe reportError . IntMap.lookup rid <$> asks rumors
  where reportError = error $ "Impossible: no rumor with id " ++ show rid
