{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Gossim.Internal.Random
       ( RandomT
       , Random
       , MonadRandom(liftRandom)
       , randomInt
       , randomRInt
       , randomDouble
       , randomBool
       , randomMaybe
       , randomMaybeM
       , pick
       , pickUniformly
       ) where

import Control.Applicative (Applicative)
import Control.Monad (liftM)
import Control.Monad.Trans (lift, MonadIO)
import Control.Monad.Identity (Identity)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT, MonadState(state))
import Control.Monad.Coroutine (Coroutine)

import System.Random.Mersenne.Pure64 (PureMT)
import qualified System.Random.Mersenne.Pure64 as Mersenne

import Gossim.Internal.Types (Prob)

newtype RandomT m a = RandomT (StateT PureMT m a)
                    deriving (Monad, MonadState PureMT,
                              Functor, Applicative, MonadIO)

type Random = RandomT Identity

class Monad m => MonadRandom m where
  liftRandom :: (PureMT -> (a, PureMT)) -> m a

instance Monad m => MonadRandom (RandomT m) where
  liftRandom = state

instance MonadRandom IO where
  liftRandom k = liftM (fst . k) Mersenne.newPureMT

instance MonadRandom m => MonadRandom (ReaderT r m) where
  liftRandom = lift . liftRandom

instance MonadRandom m => MonadRandom (StateT s m) where
  liftRandom = lift . liftRandom

instance (MonadRandom m, Functor s) => MonadRandom (Coroutine s m) where
  liftRandom = lift . liftRandom

randomInt :: MonadRandom m => m Int
randomInt = liftRandom Mersenne.randomInt

randomRInt :: MonadRandom m => (Int, Int) -> m Int
randomRInt (l, u)
  | l > u     = randomRInt (u, l)
  | otherwise = liftM bound randomInt
  where bound x = l + x `mod` (u - l + 1)

randomDouble :: MonadRandom m => m Double
randomDouble = liftRandom Mersenne.randomDouble

randomBool :: MonadRandom m => Prob -> m Bool
randomBool p = pick [(True, p), (False, 1 - p)]

randomMaybe :: MonadRandom m => Prob -> a -> m (Maybe a)
randomMaybe p = randomMaybeM p . return

randomMaybeM :: MonadRandom m => Prob -> m a -> m (Maybe a)
randomMaybeM p a = do
  just <- randomBool p
  if just
    then liftM Just a
    else return Nothing

pick :: MonadRandom m => [(a, Prob)] -> m a
pick ps = do
  r <- randomDouble
  return $ go r ps

  where go _ [] = error "Gossim.Internal.Random.pick: empty list"
        go _ [(x, _)] = x
        go r ((x, p) : rest)
          | r <= p    = x
          | otherwise = go (r - p) rest

pickUniformly :: MonadRandom m => [a] -> m a
pickUniformly []     = error "Gossim.Internal.Random.pickUniformly: empty list"
pickUniformly (p:ps) = go 1 ps p
  where go _ [] c         = return c
        go count (x:xs) c = do
          let count' = count + 1
          r <- randomRInt (1, count')
          if r == 1
            then go count' xs x
            else go count' xs c
