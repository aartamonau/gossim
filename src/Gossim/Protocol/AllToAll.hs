{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Gossim.Protocol.AllToAll (
  agent
  ) where

import Gossim (Gossim, Rumor,
               ReceiveHandler(Handler),
               receive, (!), getAgents, discovered)

import Data.Typeable (Typeable)

data Message = RemoteRumor Rumor
             deriving Typeable

agent :: Gossim ()
agent =
  receive [Handler $ \(r :: Rumor) -> handleNewRumor r,
           Handler $ \(m :: Message) -> handleMessage m]

  where handleNewRumor :: Rumor -> Gossim ()
        handleNewRumor rumor = do
          agents <- getAgents
          mapM_ (! rumor) agents

        handleMessage :: Message -> Gossim ()
        handleMessage (RemoteRumor rumor) = discovered rumor
