{-# OPTIONS_GHC -Wno-orphans #-}
module MutGraph.Impl.DijkstraStandalone (
    )
where
import Prelude
import Control.Monad.ST
import MutGraph.Dijkstra
import MutGraph.Graph
import MutContainers.Bi.Map
import MutContainers.Bi.Container
import MutContainers.Bi.List
import MutContainers.Bi.Size
import MutState.State
import MutContainers.Vector
import MutContainers.Unbox
import qualified MutContainers.Mono.Container as M
import MutContainers.Run
import MutContainers.Curry
import MutContainers.Bi.Heap

type Scanned = VectorU
type Labels = VectorU
type Queue = Heap VectorU

type GenInputs g k h e l z = (g, k)
type GenOutputs l e = (l e)
type InputsM m s g k e = (
        Mut s Scanned Bool,
        Mut s Labels e,
        Mut s (Queue k) (e, k),
        Cst s g,
        k
    )
type OutputsM m s g e = (Mut s Labels e, Cst s g)
type GenInputsM s m g k h e l z = (Cst s g, k)
type GenOutputsM s m l e = (l e)

instance (
    GraphReqs g k h e l z,
    M.UThawM g,
    DijkstraSimpleM g,
    MutToCst g
    ) => Dijkstra g where
    dijkstra _graph _source =
        runST $ runM formatInputsM runAlgoM formatOutputsM
            (_graph, _source)
        where
            formatInputsM :: (MutMonad s m) => GenInputs g k h e l z -> m (GenInputsM s m g k h e l z)
            formatInputsM (graph, source) = do
                mgraph <- M.uthawM graph
                return (cst mgraph, source)
            runAlgoM :: (MutMonad s m) => GenInputsM s m g k h e l z -> m (GenOutputsM s m l e)
            runAlgoM = uncurryN dijkstraSimpleM
            formatOutputsM :: (MutMonad s m) => GenOutputsM s m l e -> m (GenOutputs l e)
            formatOutputsM = return
    {-# INLINE dijkstra #-}

instance (
    GraphReqs g k h e l z,
    GetGraphNodeCountC g,
    Num k,
    Unbox e,
    q ~ Queue k,
    scanned ~ Scanned,
    labels ~ Labels,
    SizeOf VectorU ~ z,
    KeyOf labels ~ k,
    KeyOf scanned ~ k,
    UFreezeC Labels e,
    M.Convert (Labels e) (l e),
    DijkstraM g q scanned labels
    ) => DijkstraSimpleM g where
    dijkstraSimpleM _graph _source =
        runM formatInputsM runAlgoM formatOutputsM 
            (_graph, _source)
        where
            formatInputsM :: (MutMonad s m) => GenInputsM s m g k h e l z -> m (InputsM m s g k e)
            formatInputsM (mgraph, source) = do
                n <- getGraphNodeCountC mgraph
                queue_vec  <- replicateM n $ return (0, 0)
                queue_size <- newMutVar 0
                let queue = Heap (MutSP queue_vec, queue_size)
                labels <- replicateM n $ return 0
                scanned <- replicateM n $ return False
                return (scanned, labels, queue, mgraph, source)
            runAlgoM :: (MutMonad s m) => InputsM m s g k e -> m (OutputsM m s g e)
            runAlgoM inputs = do
                let (_, labels, _, mgraph, _) = inputs
                uncurryN dijkstraM inputs
                return (labels, mgraph)
            formatOutputsM :: (MutMonad s m) => OutputsM m s g e -> m (GenOutputsM s m l e)
            formatOutputsM (labels, _) = M.convert <$> ufreezeC labels
    {-# INLINE dijkstraSimpleM #-}
