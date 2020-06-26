{-# LANGUAGE TemplateHaskell #-}
module MutGraph.Graph (
    VertexKeyOf, EdgeKeyOf, EdgeValueOf, GraphListOf, GraphSizeOf,
    GraphReqs,

    GetGraphNodeCount, getGraphNodeCount,
    ReadGraphEdge, readGraphEdge,
    ListGraphNodes, listGraphNodes,
    ListGraphEdgesFrom, listGraphEdgesFrom,
    ListGraphEdges, listGraphEdges,
    MakeGraphFromEdges, makeGraphFromEdges,

    GetGraphNodeCountC, getGraphNodeCountC,
    ReadGraphEdgeC, readGraphEdgeC,
    WriteGraphEdgeM, writeGraphEdgeM, modGraphEdgeM,
    ListGraphNodesC, listGraphNodesC,
    ListGraphEdgesFromC,  listGraphEdgesFromC,
    ListGraphEdgesC(..),

    AddGraphEdgeM, addGraphEdgeM,
    MakeEmptyGraphM, makeEmptyGraphM,
    MakeGraphFromEdgesM, makeGraphFromEdgesM,
    ParseEdgesFromFileM, parseEdgesFromFileM,

    GetEdgeTail, getEdgeTail,
    GetEdgeHead, getEdgeHead,
    GetEdgeData, getEdgeData,
    GetEdgeKey, getEdgeKey,
    Edge(..),

    fmapGraph,
    GraphMap,
) where
import Prelude
import Control.Monad
import MutState.State
import Control.DeepSeq
import MutContainers.Unbox

type family VertexKeyOf x :: *
type family EdgeKeyOf x :: *
type family EdgeValueOf x :: *
type family GraphListOf x :: * -> *
type family GraphSizeOf x :: *

type GraphReqs g k h e l z = (
    k ~ VertexKeyOf g,
    h ~ EdgeKeyOf g,
    e ~ EdgeValueOf g,
    l ~ GraphListOf g,
    z ~ GraphSizeOf g
    )

-- immutable graph
class GetGraphNodeCount g where
    getGraphNodeCount :: (GraphReqs g k h e l z) => g -> z
class ReadGraphEdge g where
    readGraphEdge :: (GraphReqs g k h e l z) => g -> h -> Edge g
class ListGraphNodes g where
    listGraphNodes :: (GraphReqs g k h e l z) => g -> l k
class ListGraphEdgesFrom g where
    listGraphEdgesFrom :: (GraphReqs g k h e l z) => g -> k -> l (Edge g)
class ListGraphEdges g where
    listGraphEdges :: 
        (GraphReqs g k h e l z) => g -> l (Edge g)
    default listGraphEdges :: (Monad l, ListGraphNodes g, ListGraphEdgesFrom g) =>
        (GraphReqs g k h e l z) => g -> l (Edge g)
    listGraphEdges g = listGraphNodes g >>= listGraphEdgesFrom g
class MakeGraphFromEdges g list where
    makeGraphFromEdges :: (GraphReqs g k h e l z, Foldable list) => list (k, k, e) -> g

-- mutable graph
class GetGraphNodeCountC g where
    getGraphNodeCountC :: (MutMonad s m, GraphReqs g k h e l z) =>
        Cst s g -> m z
class ReadGraphEdgeC g where
    readGraphEdgeC :: (MutMonad s m, GraphReqs g k h e l z) =>
        Cst s g -> h -> m (Edge g)
class WriteGraphEdgeM g where
    writeGraphEdgeM :: (MutMonad s m, GraphReqs g k h e l z) =>
        Mut s g -> h -> e -> m ()
modGraphEdgeM :: (MutMonad s m, ReadGraphEdgeC g, WriteGraphEdgeM g, GetEdgeData (Edge g), MutToCst g)
    => Mut s g -> (EdgeValueOf (Edge g) -> EdgeValueOf (Edge g)) -> EdgeKeyOf g -> m ()
modGraphEdgeM g a h = readGraphEdgeC (cst g) h >>= \edge -> writeGraphEdgeM g h (a (getEdgeData edge))
class ListGraphNodesC g where
    listGraphNodesC :: (MutMonad s m, GraphReqs g k h e l z) =>
        Cst s g -> m (l k)
class ListGraphEdgesFromC g where
    listGraphEdgesFromC :: (MutMonad s m, GraphReqs g k h e l z) =>
        Cst s g -> k -> m (l (Edge g))
class ListGraphEdgesC g where
    listGraphEdgesC :: 
        (MutMonad s m, GraphReqs g k h e l z) => Cst s g -> m (l (Edge g))
    default listGraphEdgesC :: (Traversable l, Monad l, ListGraphNodesC g, ListGraphEdgesFromC g) =>
        (MutMonad s m, GraphReqs g k h e l z) => Cst s g -> m (l (Edge g))
    listGraphEdgesC g = join <$> (listGraphNodesC g >>= mapM (listGraphEdgesFromC g))
class AddGraphEdgeM g where
    addGraphEdgeM :: (MutMonad s m, GraphReqs g k h e l z) =>
        Mut s g -> (k, k, e) -> m h
class MakeEmptyGraphM g where
    makeEmptyGraphM :: (MutMonad s m, GraphReqs g k h e l z) => m (Mut s g)
class MakeGraphFromEdgesM g list where
    makeGraphFromEdgesM ::
        (MutMonad s m, GraphReqs g k h e l z, Foldable list) =>
        list (k, k, e) -> m (Mut s g)
    default makeGraphFromEdgesM ::  
        (MutMonad s m, GraphReqs g k h e l z, Foldable list) =>
        (AddGraphEdgeM g, MakeEmptyGraphM g) =>
        list (k, k, e) -> m (Mut s g)
    makeGraphFromEdgesM edges = do
        graph <- makeEmptyGraphM
        mapM_ (addGraphEdgeM graph) edges
        return graph
    {-# INLINE makeGraphFromEdgesM #-}
class ParseEdgesFromFileM list e k z where
    parseEdgesFromFileM :: (MutMonad s m, m ~ IO) => String -> m (list (k, k, e), z, z)


-- edge
class GetEdgeTail edge where getEdgeTail :: (k ~ VertexKeyOf edge) => edge -> k
class GetEdgeHead edge where getEdgeHead :: (k ~ VertexKeyOf edge) => edge -> k
class GetEdgeData edge where getEdgeData :: (e ~ EdgeValueOf edge) => edge -> e
class GetEdgeKey edge where getEdgeKey :: (h ~ EdgeKeyOf edge) => edge -> h

newtype Edge g = Edge (VertexKeyOf g, VertexKeyOf g, EdgeValueOf g, EdgeKeyOf g)
instance (GraphReqs g k h e l z, NFData (k, k, e, h)) => NFData (Edge g) where
    rnf (Edge (u, v, e, h)) = rnf (u, v, e, h)
    {-# INLINE rnf #-}
derivingUnbox "Edge"
    [t| forall g. (Unbox (VertexKeyOf g), Unbox (EdgeKeyOf g), Unbox (EdgeValueOf g)) => 
        Edge g -> (VertexKeyOf g, VertexKeyOf g, EdgeValueOf g, EdgeKeyOf g) |]
    [| \ (Edge edge) -> edge |]
    [| Edge |]
type instance VertexKeyOf (Edge g) = VertexKeyOf g
type instance EdgeKeyOf (Edge g) = EdgeKeyOf g
type instance EdgeValueOf (Edge g) = EdgeValueOf g

instance GetEdgeTail (Edge g) where
    getEdgeTail (Edge (u, _, _, _)) = u
    {-# INLINE getEdgeTail #-}
instance GetEdgeHead (Edge g) where
    getEdgeHead (Edge (_, v, _, _)) = v
    {-# INLINE getEdgeHead #-}
instance GetEdgeData (Edge g) where
    getEdgeData (Edge (_, _, e, _)) = e
    {-# INLINE getEdgeData #-}
instance GetEdgeKey (Edge g) where
    getEdgeKey (Edge (_, _, _, h)) = h
    {-# INLINE getEdgeKey #-}

-- graph projection
fmapGraph :: (e ~ EdgeValueOf g) => (e -> x) -> g -> GraphMap g e x
fmapGraph f g = GraphMap (g, f)
{-# INLINE fmapGraph #-}

newtype GraphMap g e x = GraphMap (g, e -> x)
type instance Mut s (GraphMap g e x) = GraphMap (Mut s g) e x
type instance Cst s (GraphMap g e x) = GraphMap (Cst s g) e x
type instance VertexKeyOf (GraphMap g e x) = VertexKeyOf g
type instance EdgeKeyOf (GraphMap g e x) = EdgeKeyOf g
type instance EdgeValueOf (GraphMap g e x) = x
type instance GraphListOf (GraphMap g e x) = GraphListOf g
type instance GraphSizeOf (GraphMap g e x) = GraphSizeOf g

instance (GetGraphNodeCount g) => GetGraphNodeCount (GraphMap g e x) where
    getGraphNodeCount (GraphMap (graph, _)) = getGraphNodeCount graph
    {-# INLINE getGraphNodeCount #-}
instance (ReadGraphEdge g, e ~ EdgeValueOf g) =>
    ReadGraphEdge (GraphMap g e x) where
    readGraphEdge (GraphMap (graph, f)) _h = 
        let Edge (u, v, e, h) = readGraphEdge graph _h
        in Edge (u, v, f e, h)
    {-# INLINE readGraphEdge #-}
instance (ListGraphNodes g) => ListGraphNodes (GraphMap g e x) where
    listGraphNodes (GraphMap (graph, _)) = listGraphNodes graph
    {-# INLINE listGraphNodes #-}
instance (GraphReqs g k h e l z, Functor l, ListGraphEdgesFrom g) =>
    ListGraphEdgesFrom (GraphMap g e x) where
    listGraphEdgesFrom (GraphMap (graph, f)) _u = 
        let l = listGraphEdgesFrom graph _u
        in fmap g l
        where g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
    {-# INLINE listGraphEdgesFrom #-}
instance (GraphReqs g k h e l z, Functor l, ListGraphEdges g) =>
    ListGraphEdges (GraphMap g e x) where
    listGraphEdges (GraphMap (graph, f)) = 
        let l = listGraphEdges graph
        in fmap g l
        where g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
    {-# INLINE listGraphEdges #-}

instance (GetGraphNodeCountC g) => GetGraphNodeCountC (GraphMap g e x) where
    getGraphNodeCountC (GraphMap (graph, _)) = getGraphNodeCountC graph
    {-# INLINE getGraphNodeCountC #-}
instance (ReadGraphEdgeC g, e ~ EdgeValueOf g) =>
    ReadGraphEdgeC (GraphMap g e x) where
    readGraphEdgeC (GraphMap (graph, f)) _h = do
        Edge (u, v, e, h) <- readGraphEdgeC graph _h
        return (Edge (u, v, f e, h))
    {-# INLINE readGraphEdgeC #-}
instance (ListGraphNodesC g) => ListGraphNodesC (GraphMap g e x) where
    listGraphNodesC (GraphMap (graph, _)) = listGraphNodesC graph
    {-# INLINE listGraphNodesC #-}
instance (GraphReqs g k h e l z, Functor l, ListGraphEdgesFromC g) =>
    ListGraphEdgesFromC (GraphMap g e x) where
    listGraphEdgesFromC (GraphMap (graph, f)) _u = 
        fmap g <$> listGraphEdgesFromC graph _u
        where g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
    {-# INLINE listGraphEdgesFromC #-}
instance (GraphReqs g k h e l z, Functor l, ListGraphEdgesC g) =>
    ListGraphEdgesC (GraphMap g e x) where
    listGraphEdgesC (GraphMap (graph, f)) = 
        fmap g <$> listGraphEdgesC graph
        where g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
    {-# INLINE listGraphEdgesC #-}
