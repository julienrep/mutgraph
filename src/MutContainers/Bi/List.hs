module MutContainers.Bi.List (
        Zip(..),
        Map(..),
        EnumFromTo(..),
        Concat(..),
        Replicate(..),

        ReplicateM(..), ReplicateMM(..),
        PushFrontM(..), PushFrontMM(..), PushFrontMC(..), 
        PushBackM(..),
        PopFrontM(..),
        PopBackM(..),
    )
where
import Prelude hiding (length, zip, map, enumFromTo)
import MutContainers.Bi.Size
import MutState.State

class Zip (l :: * -> *) a b where zip :: l a -> l b -> l (a, b)

class Map (l :: * -> *) a b where map :: (a -> b) -> l a -> l b

class EnumFromTo (l :: * -> *) a where enumFromTo :: a -> a -> l a

class Concat (l :: * -> *) a where concat :: l a -> l a -> l a

class Replicate (l :: * -> *) a where replicate :: SizeOf l -> a -> l a


class ReplicateM (l :: * -> *) a where replicateM :: (MutMonad s m) => SizeOf l -> m a -> m (Mut s l a)
class ReplicateMM (l :: * -> *) a where replicateMM :: (MutMonad s m) => SizeOf l -> m (Mut s a) -> m (Mut s l (Mut s a))
instance (forall b. ReplicateM l b) => ReplicateMM (l :: * -> *) a where replicateMM = replicateM

class PushFrontM (l :: * -> *) a where pushFrontM :: (MutMonad s m) => Mut s l a -> a -> m ()
class PushFrontMM (l :: * -> *) a where pushFrontMM :: (MutMonad s m) => Mut s l (Mut s a) -> Mut s a -> m ()
instance (forall b. PushFrontM l b) => PushFrontMM (l :: * -> *) a where pushFrontMM = pushFrontM
class PushFrontMC (l :: * -> *) a where pushFrontMC :: (MutMonad s m) => Mut s l (Cst s a) -> Cst s a -> m ()
instance (forall b. PushFrontM l b) => PushFrontMC (l :: * -> *) a where pushFrontMC = pushFrontM

class PushBackM (l :: * -> *) a where pushBackM :: (MutMonad s m) => Mut s l a -> a -> m ()

class PopFrontM (l :: * -> *) a where popFrontM :: (MutMonad s m) => Mut s l a -> m a

class PopBackM (l :: * -> *) a where popBackM :: (MutMonad s m) => Mut s l a -> m a
