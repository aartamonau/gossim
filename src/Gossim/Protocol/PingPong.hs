{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Gossim.Protocol.PingPong
       ( agent
       ) where

import Data.Text.Format (Only(Only))
import Data.Typeable (Typeable)

import Gossim (Agent, AgentId,
               getSelf, getAgents, broadcast, (!), receive,
               logInfo, logError)

data Message = Ping AgentId | Pong AgentId
             deriving Typeable

agent :: Agent ()
agent = do
  master <- isMaster
  if master
    then do
      $logInfo "I am master" ()
      loopMaster
    else do
      $logInfo "I am slave" ()
      loopSlave

  where loopMaster :: Agent ()
        loopMaster = do
          agents <- getAgents
          self <- getSelf
          $logInfo "Sending ping to {} agents" (Only $ length agents)
          broadcast agents (Ping self)
          receive handleMsg
          loopMaster
          where handleMsg :: Message -> Agent ()
                handleMsg (Pong aid) = $logInfo "Got pong from {}" (Only aid)
                handleMsg (Ping aid) = $logError "Got unexpected ping from {}" (Only aid)

        loopSlave :: Agent ()
        loopSlave = do
          receive handleMsg
          loopSlave

          where handleMsg :: Message -> Agent ()
                handleMsg (Ping aid) = do
                  $logInfo "Got ping from {}. Sending pong in response." (Only aid)
                  self <- getSelf
                  aid ! Pong self
                handleMsg (Pong aid) =
                  $logError "Got unexpected pong from {}" (Only aid)

isMaster :: Agent Bool
isMaster = do
  self <- getSelf
  agents <- getAgents

  return $ self == minimum agents
