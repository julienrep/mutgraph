module MutGraph.GraphSearch (
    VisitStatus(..),
    DfsM(..),
    BfsM(..),
    nodeNop,
    edgeNop,
) where
import Prelude
import MutContainers.Bi.Map
import MutState.State
import MutGraph.Graph

data VisitStatus = Unvisited | Visiting | Visited

nodeNop :: (MutMonad s m) => k -> m ()
nodeNop = const $ return ()
edgeNop :: (MutMonad s m) => edge -> m ()
edgeNop = const $ return ()

class DfsM g colors where
    dfsM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf colors) =>
        (k -> m ()) -> (k -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> Mut s colors VisitStatus -> Mut s g -> k -> m ()

class BfsM g colors q where
    bfsM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf colors) =>
        (k -> m ()) -> (k -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> Mut s q k -> Mut s colors VisitStatus -> Mut s g -> k -> m ()
