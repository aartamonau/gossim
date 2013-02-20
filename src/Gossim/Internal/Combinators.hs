module Gossim.Internal.Combinators
       ( send
       , (!)
       , receive
       , discovered
       , getAgents
       , getSelf
       , getRumor
       ) where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Control.Monad.Reader (asks)
import Control.Monad.Coroutine (suspend)

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import Gossim.Internal.Agent (Agent(Agent),
                              AgentEnv(agents, self, rumors),
                              Action(Send, Receive, Discovered),
                              ReceiveHandler)
import Gossim.Internal.Types (AgentId, Rumor, RumorId(RumorId))

send :: Typeable msg => AgentId -> msg -> Agent ()
send dst msg = Agent $ suspend (Send dst msg (return ()))

(!) :: Typeable msg => AgentId -> msg -> Agent ()
(!) = send

receive :: [ReceiveHandler (Agent a)] -> Agent a
receive handlers = join $ Agent $ suspend (Receive handlers return)

discovered :: Rumor -> Agent ()
discovered rumor = Agent $ suspend (Discovered rumor (return ()))

getAgents :: Agent [AgentId]
getAgents = asks agents

getSelf :: Agent AgentId
getSelf = asks self

getRumor :: RumorId -> Agent Rumor
getRumor (RumorId rid) =
  fromMaybe reportError . IntMap.lookup rid <$> asks rumors
  where reportError = error $ "Impossible: no rumor with id " ++ show rid
