module MutContainers.Mono.List (
        Zip(..),
        Map(..),
        EnumFromTo(..),
        Concat(..),
        Replicate(..),
        ToList(..),
        FromList(..),

        ReplicateM(..),
        PushFrontM(..),
        PushBackM(..),
        PopFrontM(..),
        PopBackM(..),
) where
import qualified Prelude
import MutContainers.Mono.Size
import MutState.State
import MutContainers.Mono.Map ()
import MutContainers.Any.Map

class Zip p q r where
    zip :: (a ~ ValOf p, b ~ ValOf q, (a, b) ~ ValOf r) => p -> q -> r
class Map p q where
    map :: (a ~ ValOf p, b ~ ValOf q) => (a -> b) -> p -> q
class EnumFromTo l where
    enumFromTo :: (a ~ ValOf l) => a -> a -> l 
class Concat l where
    concat :: l -> l -> l
class Replicate l where
    replicate :: (a ~ ValOf l) => SizeOf l -> a -> l
class ToList l where
    toList :: (a ~ ValOf l) => l -> [a]
class FromList l where
    fromList :: (a ~ ValOf l) => [a] -> l

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
instance Zip [a] [b] [(a, b)] where zip = Prelude.zip
instance Map [a] [b] where map = Prelude.map
