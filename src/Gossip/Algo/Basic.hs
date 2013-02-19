{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Gossip.DSL

import Control.Monad (forM_)
import Data.Typeable (Typeable)

data Message = RemoteRumor Rumor
             deriving Typeable

agent :: GossipM ()
agent =
  receive [Handler $ \(r :: Rumor) -> handleNewRumor r,
           Handler $ \(m :: Message) -> handleMessage m]

  where handleNewRumor :: Rumor -> GossipM ()
        handleNewRumor rumor = do
          agents <- getAgents
          forM_ agents $ \agent -> agent ! rumor

        handleMessage :: Message -> GossipM ()
        handleMessage (RemoteRumor rumor) = discovered rumor
