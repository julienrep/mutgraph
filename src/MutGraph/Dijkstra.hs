module MutGraph.Dijkstra (
    Dijkstra, dijkstra,
    DijkstraSimpleM, dijkstraSimpleM,
    DijkstraM, dijkstraM,
    DijkstraInitM, dijkstraInitM,
    DijkstraLoopM, dijkstraLoopM,
    DijkstraScanM, dijkstraScanM,
) where
import Prelude 
import MutGraph.Graph
import MutContainers.Bi.Map
import MutState.State
---
import Control.Monad hiding (replicateM)
import MutContainers.Bi.PriorityQueue
import MutContainers.Bi.Container
---
import Control.Monad.ST
import MutContainers.Bi.List
import MutContainers.Vector
import MutContainers.Unbox
import qualified MutContainers.Mono.Container as M
import MutContainers.Run
import MutContainers.Curry
import MutContainers.Bi.Heap
import MutContainers.Bi.Size

class Dijkstra g labels where
    dijkstra :: (GraphReqs g k h e l z, k ~ KeyOf labels, Num e, Ord e) =>
        g -> k -> labels e

class DijkstraSimpleM g labels where
    dijkstraSimpleM :: (MutMonad s m, GraphReqs g k h e l z, 
        k ~ KeyOf labels, Num e, Ord e) =>
        Cst s g -> k -> m (labels e)

class DijkstraM g q scanned labels where
    dijkstraM :: (MutMonad s m, GraphReqs g k h e l z, 
        k ~ KeyOf scanned, k ~ KeyOf labels, Num e, Ord e) => 
        Mut s scanned Bool -> Mut s labels e -> Mut s q (e, k) ->
        Cst s g -> k -> m ()

class DijkstraInitM g q scanned labels where
    dijkstraInitM :: (MutMonad s m, GraphReqs g k h e l z, 
        k ~ KeyOf scanned, k ~ KeyOf labels, Num e, Ord e) =>
        Mut s scanned Bool -> Mut s labels e -> Mut s q (e, k) ->
        Cst s g -> k -> m ()

class DijkstraLoopM g q scanned labels where
    dijkstraLoopM :: (MutMonad s m, GraphReqs g k h e l z, 
        k ~ KeyOf scanned, k ~ KeyOf labels, Num e, Ord e) => 
        Mut s scanned Bool -> Mut s labels e -> Mut s q (e, k) ->
        Cst s g -> m ()

class DijkstraScanM g q labels where
    dijkstraScanM :: (MutMonad s m, GraphReqs g k h e l z, 
        k ~ KeyOf labels, Num e, Ord e) =>
        Mut s labels e -> Mut s q (e, k) -> Cst s g -> k -> e -> m ()


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



type GenInputs g k h e l z = (g, k)
type GenOutputs labels e = (labels e)
type InputsM m s g k e labels scanned q = (
        Mut s scanned Bool,
        Mut s labels e,
        Mut s q (e, k),
        Cst s g,
        k
    )
type OutputsM m s g e labels = (Mut s labels e, Cst s g)
type GenInputsM s m g k h e l z = (Cst s g, k)
type GenOutputsM s m labels e = (labels e)

instance (
    GraphReqs g k h e l z,
    M.UThawM g,
    DijkstraSimpleM g labels,
    MutToCst g
    ) => Dijkstra g labels where
    dijkstra _graph _source =
        runST $ runM formatInputsM runAlgoM formatOutputsM
            (_graph, _source)
        where
            formatInputsM :: (MutMonad s m) =>
                GenInputs g k h e l z -> m (GenInputsM s m g k h e l z)
            formatInputsM (graph, source) = do
                mgraph <- M.uthawM graph
                return (cst mgraph, source)
            runAlgoM :: (MutMonad s m) =>
                GenInputsM s m g k h e l z -> m (GenOutputsM s m labels e)
            runAlgoM = uncurryN dijkstraSimpleM
            formatOutputsM :: (MutMonad s m) =>
                GenOutputsM s m labels e -> m (GenOutputs labels e)
            formatOutputsM = return
    {-# INLINE dijkstra #-}

type Scanned = VectorU
type Labels = VectorU
type Queue = Heap VectorU Int
type QueueVec = VectorU

instance (
    GraphReqs g k h e l z,
    GetGraphNodeCountC g,
    Num k,
    Unbox e,
    q ~ Queue,
    scanned ~ Scanned,
    labels ~ Labels,
    qvec ~ QueueVec,
    z ~ k,
    k ~ KeyOf labels,
    k ~ KeyOf scanned,
    k ~ SizeOf labels,
    k ~ SizeOf scanned,
    UFreezeC labels e,
    ReplicateM labels e,
    ReplicateM scanned Bool,
    ReplicateM qvec (e, k),
    MakeHeapM qvec q (e, k) z,
    DijkstraM g q scanned labels,
    MutToCstC labels e
    ) => DijkstraSimpleM g labels where
    dijkstraSimpleM _graph _source =
        runM formatInputsM runAlgoM formatOutputsM 
            (_graph, _source)
        where
            formatInputsM :: (MutMonad s m) =>
                GenInputsM s m g k h e l z -> 
                m (InputsM m s g k e labels scanned q)
            formatInputsM (mgraph, source) = do
                n <- getGraphNodeCountC mgraph
                queue_vec :: Mut s qvec (e, k) <- replicateM n $ return (0, 0)
                queue <- makeHeapM queue_vec (0 :: z)
                labels <- replicateM n $ return 0
                scanned <- replicateM n $ return False
                return (scanned, labels, queue, mgraph, source)
            runAlgoM :: (MutMonad s m) =>
                InputsM m s g k e labels scanned q ->
                m (OutputsM m s g e labels)
            runAlgoM inputs = do
                let (_, labels, _, mgraph, _) = inputs
                uncurryN dijkstraM inputs
                return (labels, mgraph)
            formatOutputsM :: (MutMonad s m) =>
                OutputsM m s g e labels -> m (GenOutputsM s m labels e)
            formatOutputsM (labels, _) = ufreezeC (cstC labels)
    {-# INLINE dijkstraSimpleM #-}
