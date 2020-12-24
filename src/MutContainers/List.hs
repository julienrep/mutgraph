module MutContainers.List
  ( Zip (..),
    ZipWith (..),
    Map (..),
    EnumFrom (..),
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

class ZipWith (l :: * -> *) where zipWith :: (a -> b -> c) -> l a -> l b -> l c

class Map (l :: * -> *) where map :: (a -> b) -> l a -> l b

class EnumFrom (l :: * -> *) where enumFrom :: (Enum a) => a -> l a

class EnumFromTo (l :: * -> *) where enumFromTo :: (Enum a) => a -> a -> l a

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

instance ZipWith [] where zipWith = Prelude.zipWith

instance Map [] where map = Prelude.map

instance EnumFrom [] where enumFrom = Prelude.enumFrom

instance EnumFromTo [] where enumFromTo = Prelude.enumFromTo

instance Concat [] where concat l1 l2 = l1 Prelude.++ l2

instance Replicate [] where replicate = Prelude.replicate

