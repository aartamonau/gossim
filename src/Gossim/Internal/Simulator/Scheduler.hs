{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Lens (IndexPreservingGetter,
                     makeLenses, (%~), (%=), (.~), (.=), (&), to, use, uses)

import Control.Monad (when, liftM)
import Control.Monad.State (StateT, MonadState, runStateT)
import Control.Monad.Trans (MonadTrans, lift)

import Data.Default (Default(def))
import Data.Maybe (isNothing)
import Data.Sequence (Seq, (|>), ViewL(EmptyL, (:<)), viewl)
import qualified Data.Sequence as Seq

import Gossim.Internal.Types (AgentId, Tick)

type Cost = Int

data Scheduler = Scheduler { _ticksLeft :: Tick
                           , _runnable :: Seq AgentId
                           , _blocked :: Seq AgentId
                           }
makeLenses ''Scheduler

instance Default Scheduler where
  def = Scheduler ticksPerSlice def def

newtype Sched m a = Sched { unSched :: StateT Scheduler m a }
                  deriving (Functor, Monad, MonadTrans, MonadState Scheduler)

ticksPerSlice :: Tick
ticksPerSlice = 1000

mkScheduler :: [AgentId] -> Scheduler
mkScheduler as = def & runnable .~ Seq.fromList as

addAgent :: AgentId -> Scheduler -> Scheduler
addAgent a = runnable %~ (|> a)

sched :: Monad m => Scheduler ->  Sched m a -> m (a, Scheduler)
sched s a = runStateT (unSched a) s

currentAgent :: Monad m => Sched m (Maybe AgentId)
currentAgent = use $ runnable.first

blockCurrent :: Monad m => Sched m ()
blockCurrent = do
  fr <- use $ runnable.firstRest
  case fr of
    Nothing ->
      return ()
    Just (x, xs) -> do
      blocked %= (|> x)
      runnable .= xs
      ticksLeft .= ticksPerSlice

yieldCurrent :: Monad m => Sched m ()
yieldCurrent = do
  runnable %= rotate
  ticksLeft .= ticksPerSlice

  where rotate :: Seq a -> Seq a
        rotate (viewl -> EmptyL) = Seq.empty
        rotate (viewl -> x :< xs) = xs |> x
        rotate _ = error "impossible"

run :: Monad m => m a -> Tick -> Sched m (Maybe a)
run op cost = do
  current <- currentAgent
  when (isNothing current) $
    error "run called when no runnable agent is present"

  enoughTicks <- uses ticksLeft (>= cost)
  if enoughTicks
    then liftM Just (lift op)
    else return Nothing

first :: IndexPreservingGetter (Seq a) (Maybe a)
first = to go
  where go :: Seq a -> Maybe a
        go (viewl -> EmptyL) = Nothing
        go (viewl -> x :< _) = Just x
        -- silence bogus non-exhaustive pattern warning
        go _ = error "impossible"

firstRest :: IndexPreservingGetter (Seq a) (Maybe (a, Seq a))
firstRest = to go
  where go :: Seq a -> Maybe (a, Seq a)
        go (viewl -> EmptyL) = Nothing
        go (viewl -> x :< xs) = Just (x, xs)
        go _ = error "impossible"
