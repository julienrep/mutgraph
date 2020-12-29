module MutContainers.List
  ( ReplicateM (..),
    PushFrontM (..),
    PushBackM (..),
    PopFrontM (..),
    PopBackM (..),
  )
where

import Containers.Container (SizeOf)
import MutContainers.Map
import MutState.State

class ReplicateM l where
  replicateM :: (MutMonad s m, a ~ ValOf l) => SizeOf l -> m a -> m (Mut s l)

class PushFrontM l where
  pushFrontM :: (MutMonad s m, a ~ ValOf l) => Mut s l -> a -> m ()

class PushBackM l where
  pushBackM :: (MutMonad s m, a ~ ValOf l) => Mut s l -> a -> m ()

class PopFrontM l where
  popFrontM :: (MutMonad s m, a ~ ValOf l) => Mut s l -> m a

class PopBackM l where
  popBackM :: (MutMonad s m, a ~ ValOf l) => Mut s l -> m a
