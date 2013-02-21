{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Gossim.Internal.Types
       ( AgentId(AgentId)
       , RumorId(RumorId)
       , Rumor(Rumor, rumorId, rumorSize)
       , Time
       , Prob
       ) where

import Data.Word (Word64)
import Data.Typeable (Typeable)

type Time = Word64
type Prob = Double

newtype AgentId = AgentId Int
                deriving Eq

newtype RumorId = RumorId Int
                deriving Show

data Rumor = Rumor { rumorId   :: RumorId
                   , rumorSize :: Int
                   }
           deriving Typeable
