{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Gossim.Internal.Types
       ( AgentId(AgentId, unAgentId)
       , RumorId(RumorId, unRumorId)
       , Rumor(Rumor, rumorId, rumorSize)
       , Time
       , Prob
       ) where

import Data.Monoid (mconcat)
import Data.Text.Buildable (Buildable(build))
import Data.Text.Lazy.Builder (fromText)
import Data.Typeable (Typeable)
import Data.Word (Word64)

type Time = Word64
type Prob = Double

newtype AgentId = AgentId { unAgentId :: Int }
                deriving (Eq, Typeable)

instance Buildable AgentId where
  build (AgentId aid) = mconcat [fromText "agent-", build aid]

newtype RumorId = RumorId { unRumorId :: Int }
                deriving Show

data Rumor = Rumor { rumorId   :: RumorId
                   , rumorSize :: Int
                   }
           deriving Typeable

instance Buildable Rumor where
  build (Rumor (RumorId rid) size) =
    mconcat [fromText "rumor-", build rid,
             fromText " { size = ", build size, fromText "}"]
