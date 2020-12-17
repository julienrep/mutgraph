module MutContainers.Bi.List (
        Zip(..),
        Map(..),
        EnumFromTo(..),
        Concat(..),
        Replicate(..),
        ToList(..),
        FromList(..),

        ReplicateM(..), ReplicateMM(..),
        PushFrontM(..), PushFrontMM(..), PushFrontMC(..), 
        PushBackM(..), PushBackMM(..), PushBackMC(..), 
        PopFrontM(..), PopFrontMM(..), PopFrontMC(..), 
        PopBackM(..), PopBackMM(..), PopBackMC(..), 
    )
where
import qualified Prelude
import MutContainers.Any.Size
import MutState.State

class Zip (l :: * -> *) a b where zip :: l a -> l b -> l (a, b)

class Map (l :: * -> *) a b where map :: (a -> b) -> l a -> l b

class EnumFromTo (l :: * -> *) a where enumFromTo :: a -> a -> l a

class Concat (l :: * -> *) a where concat :: l a -> l a -> l a

class Replicate (l :: * -> *) a where replicate :: SizeOf l -> a -> l a

class ToList (l :: * -> *) a where toList :: l a -> [a]

class FromList (l :: * -> *) a where fromList :: [a] -> l a

class ReplicateM (l :: * -> *) a where replicateM :: (MutMonad s m) => SizeOf l -> m a -> m (Mut s l a)
class ReplicateMM (l :: * -> *) a where replicateMM :: (MutMonad s m) => SizeOf l -> m (Mut s a) -> m (Mut s l (Mut s a))
instance (forall _a. ReplicateM l _a) => ReplicateMM (l :: * -> *) a where replicateMM = replicateM

class PushFrontM (l :: * -> *) a where pushFrontM :: (MutMonad s m) => Mut s l a -> a -> m ()
class PushFrontMM (l :: * -> *) a where pushFrontMM :: (MutMonad s m) => Mut s l (Mut s a) -> Mut s a -> m ()
instance (forall _a. PushFrontM l _a) => PushFrontMM (l :: * -> *) a where pushFrontMM = pushFrontM
class PushFrontMC (l :: * -> *) a where pushFrontMC :: (MutMonad s m) => Mut s l (Cst s a) -> Cst s a -> m ()
instance (forall _a. PushFrontM l _a) => PushFrontMC (l :: * -> *) a where pushFrontMC = pushFrontM

class PushBackM (l :: * -> *) a where pushBackM :: (MutMonad s m) => Mut s l a -> a -> m ()
class PushBackMM (l :: * -> *) a where pushBackMM :: (MutMonad s m) => Mut s l (Mut s a) -> Mut s a -> m ()
instance (forall _a. PushBackM l _a) => PushBackMM (l :: * -> *) a where pushBackMM = pushBackM
class PushBackMC (l :: * -> *) a where pushBackMC :: (MutMonad s m) => Mut s l (Cst s a) -> Cst s a -> m ()
instance (forall _a. PushBackM l _a) => PushBackMC (l :: * -> *) a where pushBackMC = pushBackM

class PopFrontM (l :: * -> *) a where popFrontM :: (MutMonad s m) => Mut s l a -> m a
class PopFrontMM (l :: * -> *) a where popFrontMM :: (MutMonad s m) => Mut s l (Mut s a) -> m (Mut s a)
instance (forall _a. PopFrontM l _a) => PopFrontMM (l :: * -> *) a where popFrontMM = popFrontM
class PopFrontMC (l :: * -> *) a where popFrontMC :: (MutMonad s m) => Mut s l (Cst s a) -> m (Cst s a)
instance (forall _a. PopFrontM l _a) => PopFrontMC (l :: * -> *) a where popFrontMC = popFrontM

class PopBackM (l :: * -> *) a where popBackM :: (MutMonad s m) => Mut s l a -> m a
class PopBackMM (l :: * -> *) a where popBackMM :: (MutMonad s m) => Mut s l (Mut s a) -> m (Mut s a)
instance (forall _a. PopBackM l _a) => PopBackMM (l :: * -> *) a where popBackMM = popBackM
class PopBackMC (l :: * -> *) a where popBackMC :: (MutMonad s m) => Mut s l (Cst s a) -> m (Cst s a)
instance (forall _a. PopBackM l _a) => PopBackMC (l :: * -> *) a where popBackMC = popBackM

-- Prelude instances
instance Zip [] a b where zip = Prelude.zip
instance Map [] a b where map = Prelude.map
