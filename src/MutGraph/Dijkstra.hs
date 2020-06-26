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

class Dijkstra g where
    dijkstra :: (GraphReqs g k h e l z, Num e, Ord e) =>
        g -> k -> l e

class DijkstraSimpleM g where
    dijkstraSimpleM :: (MutMonad s m, GraphReqs g k h e l z, Num e, Ord e) => 
        Cst s g -> k -> m (l e)

class DijkstraM g q scanned labels where
    dijkstraM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf scanned, k ~ KeyOf labels, Num e, Ord e) => 
        Mut s scanned Bool -> Mut s labels e -> Mut s q (e, k) -> Cst s g -> k -> m ()

class DijkstraInitM g q scanned labels where
    dijkstraInitM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf scanned, k ~ KeyOf labels, Num e, Ord e) =>
        Mut s scanned Bool -> Mut s labels e -> Mut s q (e, k) -> Cst s g -> k -> m ()

class DijkstraLoopM g q scanned labels where
    dijkstraLoopM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf scanned, k ~ KeyOf labels, Num e, Ord e) => 
        Mut s scanned Bool -> Mut s labels e -> Mut s q (e, k) -> Cst s g -> m ()

class DijkstraScanM g q labels where
    dijkstraScanM :: (MutMonad s m, GraphReqs g k h e l z, k ~ KeyOf labels, Num e, Ord e) =>
        Mut s labels e -> Mut s q (e, k) -> Cst s g -> k -> e -> m ()
