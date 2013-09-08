{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gossim.Internal.Agent
       ( ReceiveHandler(Handler)
       , Action (Log, Broadcast, Receive, Random, GetSelf, GetAgents)
       , Agent (Agent, unAgent)
       , bounce
       , broadcast
       , send
       , (!)
       , receive
       , receiveMany
       , getAgents
       , getSelf
       ) where

import Control.Applicative (Applicative)
import Control.Monad (join)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.Coroutine (Coroutine(resume), suspend)

import Data.Text (Text)

import Data.Dynamic (Dynamic, toDyn)
import Data.Typeable (Typeable)

import Gossim.Internal.Random (Seed, MonadRandom(liftRandom))
import Gossim.Internal.Types (AgentId)
import Gossim.Internal.Logging (MonadLogPure(doLog), Level)

data ReceiveHandler r where
  Handler :: Typeable a => (a -> Agent r) -> ReceiveHandler r

data Action s where
  Log :: Level -> Text -> s -> Action s
  Broadcast :: [AgentId] -> Dynamic -> s -> Action s
  Receive :: [ReceiveHandler a] -> (Agent a -> s) -> Action s
  Random :: (Seed -> (a, Seed)) -> (a -> s) -> Action s
  GetSelf :: (AgentId -> s) -> Action s
  GetAgents :: ([AgentId] -> s) -> Action s

instance Functor Action where
  fmap f (Log level text s) = Log level text (f s)
  fmap f (Broadcast dst msg s) = Broadcast dst msg (f s)
  fmap f (Receive handlers s) = Receive handlers (f . s)
  fmap f (Random fr s) = Random fr (f . s)
  fmap f (GetSelf s) = GetSelf (f . s)
  fmap f (GetAgents s) = GetAgents (f . s)

newtype Agent a =
  Agent { unAgent :: Coroutine Action Identity a }
  deriving (Monad, Functor, Applicative)

instance MonadRandom Agent where
  liftRandom fr = Agent $ suspend (Random fr return)

instance MonadLogPure Agent where
  doLog level text = Agent $ suspend (Log level text (return ()))


------------------------------------------------------------------------------
bounce :: Agent a -> Either (Action (Agent a)) a
bounce (Agent c) = left (fmap Agent) (runIdentity (resume c))
  where left f (Left x)  = Left (f x)
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

getAgents :: Agent [AgentId]
getAgents = Agent $ suspend (GetAgents return)

getSelf :: Agent AgentId
getSelf = Agent $ suspend (GetSelf return)
