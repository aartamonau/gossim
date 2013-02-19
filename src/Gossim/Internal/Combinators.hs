module Gossim.Internal.Combinators
       ( send
       , (!)
       , receive
       , discovered
       , getAgents
       , getSelf
       , rumorId
       , getRumor
       ) where

import Control.Applicative ((<$>))
import Control.Monad (join)
import Control.Monad.Reader (asks)
import Control.Monad.Coroutine (suspend)

import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import Gossim.Internal.Monad (Gossim(Gossim),
                              GossimEnv(agents, self, rumors),
                              Action(Send, Receive, Discovered),
                              ReceiveHandler)
import Gossim.Internal.Types (Agent, Rumor(Rumor), RumorId(RumorId))

send :: Typeable msg => Agent -> msg -> Gossim ()
send dst msg = Gossim $ suspend (Send dst msg (return ()))

(!) :: Typeable msg => Agent -> msg -> Gossim ()
(!) = send

receive :: [ReceiveHandler (Gossim a)] -> Gossim a
receive handlers = join $ Gossim $ suspend (Receive handlers return)

discovered :: Rumor -> Gossim ()
discovered rumor = Gossim $ suspend (Discovered rumor (return ()))

getAgents :: Gossim [Agent]
getAgents = asks agents

getSelf :: Gossim Agent
getSelf = asks self

rumorId :: Rumor -> RumorId
rumorId (Rumor rid) = rid

getRumor :: RumorId -> Gossim Rumor
getRumor (RumorId rid) =
  fromMaybe reportError . IntMap.lookup rid <$> asks rumors
  where reportError = error $ "Impossible: no rumor with id " ++ show rid
