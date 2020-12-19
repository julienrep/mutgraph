module MutContainers.List
  ( Zip (..),
    Map (..),
    EnumFromTo (..),
    Concat (..),
    Replicate (..),
    ToList (..),
    FromList (..),
    ReplicateM (..),
    PushFrontM (..),
    PushBackM (..),
    PopFrontM (..),
    PopBackM (..),
  )
where

import MutState.State
import qualified Prelude
import Prelude (Enum)
import MutContainers.Map
import MutContainers.Size

class Zip (l :: * -> *) where zip :: l a -> l b -> l (a, b)

class Map (l :: * -> *) where map :: (a -> b) -> l a -> l b

class EnumFromTo (l :: * -> *) where enumFromTo :: (Enum a) => a -> a -> l a -- TODO generalize

class Concat (l :: * -> *) where concat :: l a -> l a -> l a

class Replicate (l :: * -> *) where replicate :: SizeOf l -> a -> l a

class ToList (l :: * -> *) where toList :: l a -> [a]

class FromList (l :: * -> *) where fromList :: [a] -> l a

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

-- Prelude instances
instance Zip [] where zip = Prelude.zip

instance Map [] where map = Prelude.map
