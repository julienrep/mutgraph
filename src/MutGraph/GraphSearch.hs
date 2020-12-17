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
--
import Control.Monad hiding (replicateM)
import MutContainers.Bi.Container
import MutContainers.Bi.List
import MutContainers.Any.Map
import MutContainers.Any.Size

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

instance (
        GraphReqs g k h e l z,
        KeyOf colors ~ k,
        SizeOf colors ~ z,
        ReplicateM colors VisitStatus,
        WriteM colors VisitStatus,
        ReadC colors VisitStatus,
        MutToCst2 colors VisitStatus,
        Foldable l,
        ListGraphNodesC g,
        ListGraphEdgesFromC g,
        MutToCst g
    ) => DfsM g colors where
    dfsM preNode postNode preEdge postEdge backEdge crossEdge colors graph _u = do
        listGraphNodesC (cst graph) >>= mapM_ (\k -> writeM colors k Unvisited)
        loop _u where
            loop u = do
                writeM colors u Visiting
                preNode u
                listGraphEdgesFromC (cst graph) u >>=
                    mapM_  (\edge -> do
                    let v = getEdgeHead edge
                    status <- readC (c2 colors) v
                    case status of
                        Unvisited -> do
                            preEdge edge
                            loop v
                            postEdge edge
                        Visiting -> backEdge edge
                        Visited -> crossEdge edge
                    )
                postNode u
                writeM colors u Visited
    {-# INLINE dfsM #-}

instance (
        GraphReqs g k h e l z,
        KeyOf colors ~ k,
        SizeOf colors ~ z,
        WriteM colors VisitStatus,
        ReadC colors VisitStatus,
        ReplicateM colors VisitStatus,
        MutToCst2 colors VisitStatus,
        EmptyM q k,
        IsEmptyC q k,
        PushBackM q k,
        PopFrontM q k,
        MutToCst2 q k,
        Foldable l,
        ListGraphNodesC g,
        ListGraphEdgesFromC g,
        MutToCst g
    ) => BfsM g colors q where
    bfsM preNode postNode ascEdge descEdge queue colors graph _u = do
        listGraphNodesC (cst graph) >>= mapM_ (\k -> writeM colors k Unvisited)
        emptyM queue
        pushBackM queue _u
        writeM colors _u Visiting
        loop where
            loop = do
                empty <- isEmptyC (c2 queue)
                unless empty $ do
                    u <- popFrontM queue
                    preNode u
                    listGraphEdgesFromC (cst graph) u >>=
                        mapM_ (\edge -> do
                            let v = getEdgeHead edge
                            status <- readC (c2 colors) v
                            case status of
                                Unvisited -> do
                                    ascEdge edge
                                    pushBackM queue v
                                    writeM colors v Visiting
                                Visiting -> descEdge edge
                                Visited -> descEdge edge
                            )
                    postNode u
                    writeM colors u Visited
                    loop
    {-# INLINE bfsM #-}
