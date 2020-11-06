module MutContainers.Bi.PriorityQueue (
    ExtractMinM(..),
    InsertValM(..),
)
where
import Prelude
import MutState.State

class ExtractMinM (q :: * -> *) a where
    extractMinM :: (MutMonad s m, Ord a) => Mut s q a -> m a 

-- extractMinPreM queue = do
--     e <- isEmptyC queue
--     assert (not e)
--     $ return True

class InsertValM (q :: * -> *) a where
    insertValM :: (MutMonad s m, Ord a) => Mut s q a -> a -> m ()
