module Gossim
       ( module Gossim.Internal.Types
       , module Gossim.Internal.Agent
       , module Gossim.Internal.Combinators
       ) where

import Gossim.Internal.Types (AgentId, RumorId, Rumor(rumorId))
import Gossim.Internal.Agent (ReceiveHandler(Handler), Agent)
import Gossim.Internal.Combinators (send, (!), receive, discovered,
                                    getAgents, getSelf, getRumor)
