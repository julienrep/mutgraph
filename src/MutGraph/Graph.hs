module MutGraph.Graph (
    VertexKeyOf, EdgeKeyOf, EdgeValueOf, GraphListOf, GraphSizeOf,
    GraphReqs,

    GetGraphNodeCount(..),
    ReadGraphEdge(..),
    ListGraphNodes(..),
    ListGraphEdgesFrom(..),
    ListGraphEdges(..),
    MakeGraphFromEdges(..),

    GetGraphNodeCountC(..),
    ReadGraphEdgeC(..),
    WriteGraphEdgeM(..), modGraphEdgeM,
    ListGraphNodesC(..),
    ListGraphEdgesFromC(..),
    ListGraphEdgesC(..),
    AddGraphEdgeM(..),
    MakeEmptyGraphM(..),
    MakeGraphFromEdgesM(..),
    ParseEdgesFromFileM(..),

    getEdgeTail,
    getEdgeHead,
    getEdgeData,
    getEdgeKey,
    Edge(..),

    fmapGraph,
    GraphMap,
) where
import Containers.Prelude
import MutState.State

type family VertexKeyOf g :: *
type family EdgeKeyOf g :: *
type family EdgeValueOf g :: *
type family GraphListOf g :: * -> *
type family GraphSizeOf g :: *

type GraphReqs g k h e l z = (
    k ~ VertexKeyOf g, -- information to locate a vertex in a graph
    h ~ EdgeKeyOf g,   -- information to locate an edge in a graph
    e ~ EdgeValueOf g, -- represents what data an edge holds
    l ~ GraphListOf g, -- a list-like type used for interfacing. This is not necessarily the type used to represent the graph.
    Traversable l, Monad l,
    z ~ GraphSizeOf g,  -- a numeric type used for basic numerical questions about the graph (how many nodes, edges, ...)
    Num z
    )
-- types families and GraphReqs are used, because g is monokinded.
-- a graph g is monokinded because it has mutable uses.
-- mutable types are only managed monokindedly as a cleaner design choice.

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
    default listGraphEdges :: (ListGraphNodes g, ListGraphEdgesFrom g) =>
        (GraphReqs g k h e l z) => g -> l (Edge g)
    listGraphEdges g = listGraphNodes g >>= listGraphEdgesFrom g
class MakeGraphFromEdges g list where
    makeGraphFromEdges :: (GraphReqs g k h e l z, Foldable list) =>
        list (k, k, e) -> g

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
modGraphEdgeM :: (MutMonad s m, GraphReqs g k h e l z, 
    ReadGraphEdgeC g, WriteGraphEdgeM g, MutToCst g) => 
    Mut s g -> (e -> e) -> h -> m ()
modGraphEdgeM g a h = readGraphEdgeC (cst g) h >>= f where
    f edge = writeGraphEdgeM g h (a (getEdgeData edge))
class ListGraphNodesC g where
    listGraphNodesC :: (MutMonad s m, GraphReqs g k h e l z) =>
        Cst s g -> m (l k)
class ListGraphEdgesFromC g where
    listGraphEdgesFromC :: (MutMonad s m, GraphReqs g k h e l z) =>
        Cst s g -> k -> m (l (Edge g))
class ListGraphEdgesC g where
    listGraphEdgesC :: 
        (MutMonad s m, GraphReqs g k h e l z) => Cst s g -> m (l (Edge g))
    default listGraphEdgesC :: (ListGraphNodesC g, ListGraphEdgesFromC g) =>
        (MutMonad s m, GraphReqs g k h e l z) => Cst s g -> m (l (Edge g))
    listGraphEdgesC g = 
        join <$> (listGraphNodesC g >>= mapM (listGraphEdgesFromC g))
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
class ParseEdgesFromFileM parser list e k z where
    parseEdgesFromFileM :: (MutMonad s m, Foldable list, m ~ IO) =>
        parser -> m (Maybe (list (k, k, e), z, z))


-- edge
newtype Edge g = Edge (VertexKeyOf g, VertexKeyOf g, EdgeValueOf g, EdgeKeyOf g)
type instance VertexKeyOf (Edge g) = VertexKeyOf g
type instance EdgeKeyOf (Edge g) = EdgeKeyOf g
type instance EdgeValueOf (Edge g) = EdgeValueOf g
getEdgeTail :: Edge g -> VertexKeyOf g
getEdgeTail (Edge (u, _, _, _)) = u
{-# INLINE getEdgeTail #-}
getEdgeHead :: Edge g -> VertexKeyOf g
getEdgeHead (Edge (_, v, _, _)) = v
{-# INLINE getEdgeHead #-}
getEdgeData :: Edge g -> EdgeValueOf g
getEdgeData (Edge (_, _, e, _)) = e
{-# INLINE getEdgeData #-}
getEdgeKey :: Edge g -> EdgeKeyOf g
getEdgeKey (Edge (_, _, _, h)) = h
{-# INLINE getEdgeKey #-}

-- below code simulates a Functor by attaching a function to a graph
-- and adds an api layer that tampers outputs with the function.
-- Functor instance is not possible because g is monokinded because it can be mutable
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
    readGraphEdge (GraphMap (graph, f)) _h = Edge (u, v, f e, h)
        where Edge (u, v, e, h) = readGraphEdge graph _h
    {-# INLINE readGraphEdge #-}
instance (ListGraphNodes g) => ListGraphNodes (GraphMap g e x) where
    listGraphNodes (GraphMap (graph, _)) = listGraphNodes graph
    {-# INLINE listGraphNodes #-}
instance (GraphReqs g k h e l z, ListGraphEdgesFrom g) =>
    ListGraphEdgesFrom (GraphMap g e x) where
    listGraphEdgesFrom (GraphMap (graph, f)) _u = fmap g l
        where
        l = listGraphEdgesFrom graph _u
        g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
    {-# INLINE listGraphEdgesFrom #-}
instance (GraphReqs g k h e l z, ListGraphEdges g) =>
    ListGraphEdges (GraphMap g e x) where
    listGraphEdges (GraphMap (graph, f)) = fmap g l
        where
        l = listGraphEdges graph
        g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
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
instance (GraphReqs g k h e l z, ListGraphEdgesFromC g) =>
    ListGraphEdgesFromC (GraphMap g e x) where
    listGraphEdgesFromC (GraphMap (graph, f)) _u = 
        fmap g <$> listGraphEdgesFromC graph _u
        where g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
    {-# INLINE listGraphEdgesFromC #-}
instance (GraphReqs g k h e l z, ListGraphEdgesC g) =>
    ListGraphEdgesC (GraphMap g e x) where
    listGraphEdgesC (GraphMap (graph, f)) = fmap g <$> listGraphEdgesC graph
        where g (Edge (u, v, e, h)) = Edge (u, v, f e, h)
    {-# INLINE listGraphEdgesC #-}
