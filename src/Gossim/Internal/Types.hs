{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Gossim.Internal.Types
       ( AgentId (AgentId)
       , RumorId (RumorId)
       , Rumor (Rumor)
       ) where

import Data.Typeable (Typeable)

newtype AgentId = AgentId Int
                deriving Eq

newtype RumorId = RumorId Int
                deriving Show

data Rumor = Rumor RumorId
           deriving Typeable
