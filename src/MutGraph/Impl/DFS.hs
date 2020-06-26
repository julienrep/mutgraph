{-# OPTIONS_GHC -Wno-orphans #-}
module MutGraph.Impl.DFS (
) where
import Prelude
import MutContainers.Bi.Map
import MutContainers.Bi.List
import MutContainers.Bi.Size
import MutState.State
import MutGraph.Graph
import MutGraph.DFS
import MutContainers.Vector

type Colors = VectorU

instance (
        GraphReqs g k h e l z,
        GetGraphNodeCountC g,
        KeyOf Colors ~ k,
        SizeOf Colors ~ z,
        ReplicateM Colors VisitStatus,
        DfsLoopM g Colors,
        MutToCst g
    ) => DfsM g where
    dfsM graph preNode postNode preEdge postEdge backEdge crossEdge u = do
        n <- getGraphNodeCountC (cst graph)
        colors :: Mut s Colors VisitStatus <- replicateM n $ return Unvisited
        dfsLoopM colors graph preNode postNode preEdge postEdge backEdge crossEdge u
    {-# INLINE dfsM #-}

instance (
        GraphReqs g k h e l z,
        ListGraphEdgesFromC g,
        WriteM colors VisitStatus,
        ReadC colors VisitStatus,
        Foldable l,
        MutToCst g,
        MutToCstC colors VisitStatus
    ) => DfsLoopM g colors where
    dfsLoopM colors graph preNode postNode preEdge postEdge backEdge crossEdge = loop
        where loop u = do
                preNode u
                writeM colors u Visiting
                listGraphEdgesFromC (cst graph) u >>=
                    mapM_  (\edge -> do
                    let v = getEdgeHead edge
                    status <- readC (cstC colors) v
                    case status of
                        Unvisited -> do
                            preEdge edge
                            loop v
                            postEdge edge
                        Visiting -> backEdge edge
                        Visited -> crossEdge edge
                    )
                writeM colors u Visited
                postNode u
    {-# INLINE dfsLoopM #-}

instance (
        GraphReqs g k h e l z,
        ListGraphNodesC g,
        WriteM colors VisitStatus,
        Foldable l,
        MutToCst g
    ) => DfsInitM g colors where
    dfsInitM colors graph = listGraphNodesC (cst graph) >>= mapM_ (\u -> writeM colors u Unvisited)
    {-# INLINE dfsInitM #-}
