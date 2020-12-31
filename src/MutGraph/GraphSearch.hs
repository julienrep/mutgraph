module MutGraph.GraphSearch (
    VisitStatus(..),
    DfsM(..),
    BfsM(..),
    nodeNop,
    edgeNop,
) where
import Containers.Prelude
import MutState.State
import MutGraph.Graph
import MutContainers.Container
import MutContainers.List
import MutContainers.Map
import Containers.Container

data VisitStatus = Unvisited | Visiting | Visited

nodeNop :: (MutMonad s m) => k -> m ()
nodeNop = const $ return ()
edgeNop :: (MutMonad s m) => edge -> m ()
edgeNop = const $ return ()

class DfsM g colors where
    dfsM :: (MutMonad s m, GraphReqs g k h e l z, 
        k ~ KeyOf colors, ValOf colors ~ VisitStatus) =>
        (k -> m ()) -> (k -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> Mut s colors -> Mut s g -> k -> m ()

class BfsM g colors q where
    bfsM :: (MutMonad s m, GraphReqs g k h e l z,
        k ~ KeyOf colors, ValOf colors ~ VisitStatus,
        ValOf q ~ k) =>
        (k -> m ()) -> (k -> m ())
        -> (Edge g -> m ()) -> (Edge g -> m ())
        -> Mut s q -> Mut s colors -> Mut s g -> k -> m ()

instance (
        GraphReqs g k h e l z,
        SizeOf colors ~ z,
        ReplicateM colors,
        WriteM colors,
        ReadC colors,
        MutToCst colors,
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
                    status <- readC (cst colors) v
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
        SizeOf colors ~ z,
        WriteM colors,
        ReadC colors,
        ReplicateM colors,
        MutToCst colors,
        EmptyM q,
        IsEmptyC q,
        PushBackM q,
        PopFrontM q,
        MutToCst q,
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
                empty <- isEmptyC (cst queue)
                unless empty $ do
                    u <- popFrontM queue
                    preNode u
                    listGraphEdgesFromC (cst graph) u >>=
                        mapM_ (\edge -> do
                            let v = getEdgeHead edge
                            status <- readC (cst colors) v
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
