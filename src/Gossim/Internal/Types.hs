{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Gossim.Internal.Types
       ( AgentId(AgentId, unAgentId)
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
                deriving (Eq, Ord, Typeable)

instance Buildable AgentId where
  build (AgentId aid) = mconcat [fromText "agent-", build aid]
