{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Gossim.Internal.Types
       ( Agent (Agent)
       , RumorId (RumorId)
       , Rumor (Rumor)
       ) where

import Data.Typeable (Typeable)

newtype Agent = Agent Int
              deriving Eq

newtype RumorId = RumorId Int
                deriving Show

data Rumor = Rumor RumorId
           deriving Typeable
