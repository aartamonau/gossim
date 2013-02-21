{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gossim.Protocol.AllToAll
       ( agent
       ) where

import Control.Monad (forever)

import Gossim (Agent, Rumor,
               ReceiveHandler(Handler),
               receive, (!), getAgents, discovered)

import Data.Typeable (Typeable)

data Message = RemoteRumor Rumor
             deriving Typeable

agent :: Agent ()
agent =
  forever $
    receive [Handler $ \(r :: Rumor) -> handleNewRumor r,
             Handler $ \(m :: Message) -> handleMessage m]

  where handleNewRumor :: Rumor -> Agent ()
        handleNewRumor rumor = do
          agents <- getAgents
          mapM_ (! rumor) agents

        handleMessage :: Message -> Agent ()
        handleMessage (RemoteRumor rumor) = discovered rumor
