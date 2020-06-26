{-# OPTIONS_GHC -Wno-orphans #-}
module MutGraph.Impl.Dijkstra (
    )
where
import Prelude
import Control.Monad hiding (replicateM)
import MutGraph.Dijkstra
import MutContainers.Bi.PriorityQueue
import MutGraph.Graph
import MutContainers.Bi.Map
import MutContainers.Bi.Container
import MutState.State

instance (
    GraphReqs g k h e l z,
    DijkstraInitM g q scanned labels,
    DijkstraLoopM g q scanned labels
    ) => DijkstraM g q scanned labels where
    dijkstraM scanned labels queue graph source = do
        dijkstraInitM scanned labels queue graph source
        dijkstraLoopM scanned labels queue graph
    {-# INLINE dijkstraM #-}

instance (
    GraphReqs g k h e l z,
    WriteM scanned Bool,
    WriteM labels e,
    Bounded e,
    EmptyM q (e, k),
    Ord k,
    InsertValM q (e, k),
    Traversable l,
    ListGraphNodesC g
    ) => DijkstraInitM g q scanned labels where
    dijkstraInitM scanned labels queue graph source = do
        let infinity = maxBound
        listGraphNodesC graph >>=
            mapM_ (\k -> do
                writeM labels k infinity
                writeM scanned k False)
        writeM labels source 0
        emptyM queue
        insertValM queue (0, source)
    {-# INLINE dijkstraInitM #-}

instance (
    GraphReqs g k h e l z,
    ReadC scanned Bool,
    WriteM scanned Bool, MutToCstC scanned Bool,
    DijkstraScanM g q labels,
    ExtractMinM q (e, k),
    Ord k,
    IsEmptyC q (e, k),
    MutToCstC q (e, k)
    ) => DijkstraLoopM g q scanned labels where
    dijkstraLoopM scanned labels queue graph = loop where
        loop = do
            empty <- isEmptyC (cstC queue)
            unless empty $ do
                (du, u) <- extractMinM queue
                wasScanned <- readC (cstC scanned) u
                unless wasScanned $ do
                    dijkstraScanM labels queue graph u du 
                    writeM scanned u True
                loop
    {-# INLINE dijkstraLoopM #-}

instance (
    GraphReqs g k h e l z,
    Ord k,
    InsertValM q (e, k),
    Traversable l,
    ReadC labels e,
    MutToCstC labels e,
    WriteM labels e,
    ListGraphEdgesFromC g
    ) => DijkstraScanM g q labels where
    dijkstraScanM labels queue graph u du = 
        listGraphEdgesFromC graph u >>=
        mapM_ (\edge -> do
            let v = getEdgeHead edge
            let luv = getEdgeData edge
            dv <- readC (cstC labels) v
            let dnew = du + luv
            when (dv > dnew) $ do
                writeM labels v dnew
                insertValM queue (dnew, v))
    {-# INLINE dijkstraScanM #-}
