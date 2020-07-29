module MutContainers.Mono.PriorityQueue (
    ExtractMinM(..),
    InsertValM(..),
)
where
import Prelude
import MutState.State
import MutContainers.Mono.Map

class ExtractMinM q where
    extractMinM :: (MutMonad s m, a ~ ValOf q, Ord a) => Mut s q -> m a 
class InsertValM q where
    insertValM :: (MutMonad s m, a ~ ValOf q, Ord a) => Mut s q -> a -> m ()
