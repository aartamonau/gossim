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
          nodes <- getNodes
          forM_ nodes $ \node -> node ! rumor

        handleMessage :: Message -> GossipM ()
        handleMessage (RemoteRumor rumor) = discovered rumor
