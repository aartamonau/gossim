{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gossim.Internal.Monad
       ( ReceiveHandler(Handler)
       , Action (Send, Receive, Discovered)
       , GossimEnv (GEnv, self, agents, rumors)
       , Gossim (Gossim, unGossim)
       ) where

import Control.Monad.Trans (MonadTrans(lift))
import Control.Monad.Reader (Reader, MonadReader(ask, local, reader))
import Control.Monad.Coroutine (Coroutine, mapMonad)

import Data.IntMap.Strict (IntMap)
import Data.Typeable (Typeable)

import Gossim.Internal.Types (Agent, Rumor)

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

data GossimEnv = GEnv { self   :: Agent
                      , agents :: [Agent]
                      , rumors :: IntMap Rumor
                      }

newtype Gossim a = Gossim { unGossim :: Coroutine Action (Reader GossimEnv) a }
                  deriving (Monad, Functor)

instance MonadReader GossimEnv Gossim where
  ask     = Gossim $ lift ask
  reader  = Gossim . lift . reader
  local f = Gossim . mapMonad (local f) . unGossim
