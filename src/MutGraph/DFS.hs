module MutGraph.DFS (
    DfsM(..),
    DfsLoopM(..),
    DfsInitM(..),
    nodeNop,
    edgeNop,
    VisitStatus(..),
) where
import Prelude
import MutContainers.Bi.Map
import MutState.State
import MutGraph.Graph

data VisitStatus = Unvisited | Visiting | Visited deriving (Enum)

class DfsM g where
    dfsM :: (MutMonad s m, GraphReqs g k h e l z) =>
        Mut s g
        -> (k -> m ()) -> (k -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> k -> m ()

class DfsLoopM g colors where
    dfsLoopM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf colors) =>
        Mut s colors VisitStatus -> Mut s g
        -> (k -> m ()) -> (k -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> k -> m ()

class DfsInitM g colors where
    dfsInitM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf colors) =>
        Mut s colors VisitStatus -> Mut s g -> m ()

nodeNop :: (MutMonad s m) => k -> m ()
nodeNop = const $ return ()
edgeNop :: (MutMonad s m) => edge -> m ()
edgeNop = const $ return ()
