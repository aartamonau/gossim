{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Gossim.DSL

import Control.Monad (forM_)
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
          forM_ agents $ \agent -> agent ! rumor

        handleMessage :: Message -> Gossim ()
        handleMessage (RemoteRumor rumor) = discovered rumor
