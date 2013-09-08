{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Gossim.Protocol.PingPong
       ( agent
       ) where

import Data.Typeable (Typeable)

import Gossim (Agent, AgentId, Only(Only),
               getSelf, getAgents, broadcast, (!), receive,
               infoM, errorM)

data Message = Ping AgentId | Pong AgentId
             deriving Typeable

agent :: Agent ()
agent = do
  master <- isMaster
  if master
    then do
      infoM "I am master" ()
      loopMaster
    else do
      infoM "I am slave" ()
      loopSlave

  where loopMaster :: Agent ()
        loopMaster = do
          agents <- getAgents
          self <- getSelf
          infoM "Sending ping to {} agents" (Only $ length agents)
          broadcast agents (Ping self)
          receive handleMsg
          loopMaster
          where handleMsg :: Message -> Agent ()
                handleMsg (Pong aid) = infoM "Got pong from {}" (Only aid)
                handleMsg (Ping aid) = errorM "Got unexpected ping from {}" (Only aid)

        loopSlave :: Agent ()
        loopSlave = do
          receive handleMsg
          loopSlave

          where handleMsg :: Message -> Agent ()
                handleMsg (Ping aid) = do
                  infoM "Got ping from {}. Sending pong in response." (Only aid)
                  self <- getSelf
                  aid ! Pong self
                handleMsg (Pong aid) =
                  errorM "Got unexpected pong from {}" (Only aid)

isMaster :: Agent Bool
isMaster = do
  self <- getSelf
  agents <- getAgents

  return $ self == minimum agents
