module MutGraph.AdjacencyList2 (
    AdjList,
) where
import Prelude (Num(..), (.), (<$>), ($))
import Data.Foldable hiding (concat)
import Data.Traversable
import Data.Ord
import Control.Monad
import Control.Monad.ST
import Control.DeepSeq
import MutGraph.Graph
import MutContainers.Bi.Container
import MutContainers.Bi.Map
import MutContainers.Bi.Size
import MutContainers.Bi.List
import MutState.State
import qualified MutContainers.Mono.Container as M
import qualified MutContainers.Mono.Size as M
import MutContainers.Any.Map
import MutContainers.Any.Size

newtype AdjList 
    (l :: * -> *) (i :: *) (w :: * -> *) (v :: * -> *) (k :: *) (e :: *) = 
    AdjList (w (v (k, e)))
type instance VertexKeyOf (AdjList l i w v k e) = k
type instance EdgeKeyOf (AdjList l i w v k e) = 
    (VertexKeyOf (AdjList l i w v k e), i)
type instance EdgeValueOf (AdjList l i w v k e) = e
type instance GraphListOf (AdjList l i w v k e) = l
type instance GraphSizeOf (AdjList l i w v k e) = k
instance (NFData (w (v (k, e)))) => NFData (AdjList l i w v k e) where
  rnf (AdjList vle) = rnf vle
  {-# INLINE rnf #-}

type instance Mut s (AdjList l i w v k e) = 
    AdjList l i (MM (Mut s w)) (MP (Mut s v)) k e
type instance Cst s (AdjList l i w v k e) = 
    AdjList l i (MM (Cst s w)) (MP (Cst s v)) k e

type AdjListReqs g k h e l z i w v = (
    g ~ AdjList l i w v k e,
    GraphReqs g k h e l z,
    KeyOf w ~ k,
    KeyOf v ~ i,
    h ~ (k, i),
    SizeOf w ~ z,
    GetSize w (v (k, e)),
    SizeOf v ~ i,
    GetSize v (k, e),
    ReadAt w (v (k, e)),
    ReadAt v (k, e),
    Num k,
    Num i,
    EnumFromTo l k,
    Convert v l (Edge g),
    Map v ((k, e), h) (Edge g),
    Map v i h,
    Zip v (k, e) h,
    EnumFromTo v i
    )

type MutAdjListReqs g k h e l z i w v = (
    AdjListReqs g k h e l z i w v,
    MakeNew w (v (k, e)),
    MakeNewM v (k, e),
    MakeNewMM w (MP v (k, e)),
    GetSizeCC w (MP v (k, e)),
    SetSizeMM w (MP v (k, e)),
    GrowSizeMM w (MP v (k, e)),
    ShrinkSizeMM w (MP v (k, e)),
    EnsureSizeMM w (MP v (k, e)),
    ReadCC w (MP v (k, e)),
    ReadCM w (MP v (k, e)),
    WriteMM w (MP v (k, e)),
    GetSizeC v (k, e),
    ReadC v (k, e),
    WriteM v (k, e),
    UFreezeC v (k, e),
    UThawM v (k, e),
    Replicate v (k, e),
    Concat v (k, e)
    )

instance (AdjListReqs g k h e l z i w v) => 
    GetGraphNodeCount (AdjList l i w v k e) where
    getGraphNodeCount (AdjList w) = getSize w
    {-# INLINE getGraphNodeCount #-}

instance (AdjListReqs g k h e l z i w v) => 
    ReadGraphEdge (AdjList l i w v k e) where
    readGraphEdge (AdjList w) h = Edge (u, k, e, h)
        where
        (u, i) = h
        (k, e) = (w `at` u) `at` i
    {-# INLINE readGraphEdge #-}

instance (AdjListReqs g k h e l z i w v, GetGraphNodeCount g) =>
    ListGraphNodes (AdjList l i w v k e) where
    listGraphNodes g = enumFromTo 0 (getGraphNodeCount g - 1)
    {-# INLINE listGraphNodes #-}

instance (AdjListReqs g k h e l z i w v, GetGraphNodeCount g) =>
    ListGraphEdgesFrom (AdjList l i w v k e) where
    listGraphEdgesFrom (AdjList w) u =
            (convert :: v (Edge g) -> l (Edge g)) (
            map (\((k, e), h) -> Edge (u, k, e, h))
            (zip v (map (u,) (enumFromTo 0 (getSize v - 1))))
            )
        where
        v = w `at` u
    {-# INLINE listGraphEdgesFrom #-}

-- instance (AdjListReqs g k h e l z i w v,
--     Monoid (l (Edge g)), 
--     MonoFoldable (l k), Element (l k) ~ k,
--     ListGraphNodes g, ListGraphEdgesFrom g
--     ) => ListGraphEdges (AdjList l i w v k e) where
--     listGraphEdges g = ofoldMap (listGraphEdgesFrom g) (listGraphNodes g)
--     {-# INLINE listGraphEdges #-}

instance (AdjListReqs g k h e l z i w v, Monad l) => 
    ListGraphEdges (AdjList l i w v k e)

instance (MutAdjListReqs g k h e l z i w v) =>
    GetGraphNodeCountC (AdjList l i w v k e) where
    getGraphNodeCountC (AdjList (MM w)) = getSizeCC w
    {-# INLINE getGraphNodeCountC #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    ReadGraphEdgeC (AdjList l i w v k e) where
    readGraphEdgeC (AdjList (MM w)) h = do
        let (u, i) = h
        MP v <- readCC w u
        (k, e) <- readC v i
        return (Edge (u, k, e, h))
    {-# INLINE readGraphEdgeC #-}

instance (MutAdjListReqs g k h e l z i w v, 
    MutToCst2 w (MP v (k, e)),
    MutToCst2 v (k, e)) =>
    WriteGraphEdgeM (AdjList l i w v k e) where
    writeGraphEdgeM (AdjList (MM w)) (u, i) e = readCM (c2M w) u >>= 
        \(MP v) -> modifyM v i (\(k, _) -> (k, e))
    {-# INLINE writeGraphEdgeM #-}

instance (MutAdjListReqs g k h e l z i w v, GetGraphNodeCountC g) => 
    ListGraphNodesC (AdjList l i w v k e) where
    listGraphNodesC g = do
        n <- getGraphNodeCountC g
        return (enumFromTo 0 (n - 1))
    {-# INLINE listGraphNodesC #-}


instance (MutAdjListReqs g k h e l z i w v) =>
    ListGraphEdgesFromC (AdjList l i w v k e) where
    listGraphEdgesFromC (AdjList (MM w)) u = do
        MP mv <- readCC w u
        v <- ufreezeC mv
        return  ( (convert :: v (Edge g) -> l (Edge g)) 
                (map (\((k, e), h) -> Edge (u, k, e, h))
                (zip v (map (u,) (enumFromTo 0 (getSize v - 1)))))
            )
    {-# INLINE listGraphEdgesFromC #-}

-- instance (MutAdjListReqs g k h e l z i w v,
--     MakeNew l (Edge g),
--     Concat l (Edge g), 
--     MonoFoldable (l k), Element (l k) ~ k,
--     ListGraphNodesC g, ListGraphEdgesFromC g
--     ) => ListGraphEdgesC (AdjList l i w v k e) where
--     listGraphEdgesC g = listGraphNodesC g >>=
--             ofoldlM (\v k -> do
--                 edges <- listGraphEdgesFromC g k
--                 return (concat v edges)
--                 ) makeNew
--     {-# INLINE listGraphEdgesC #-}

instance (MutAdjListReqs g k h e l z i w v, Traversable l, Monad l) => 
    ListGraphEdgesC (AdjList l i w v k e)

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    M.EnsureSizeM (SizeViaNodes g),
    MutToCst2 w (MP v (k, e)),
    MutToCst2 v (k, e)
    ) => AddGraphEdgeM (AdjList l i w v k e) where
    addGraphEdgeM graph (u, u', e) = do
        M.ensureSizeM (SizeViaNodes graph) (u' + 1)
        let (AdjList (MM w)) = graph
        MP v <- readCM (c2M w) u
        h <- (u,) <$> getSizeC (c2 v)
        fv <- ufreezeC (c2 v)
        nv <- uthawM (concat fv (replicate 1 (u', e)))
        writeMM w u (MP nv)
        return h
    {-# INLINE addGraphEdgeM #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    M.MakeNewM (AdjList l i w v k e) where
    makeNewM = makeNewMM >>= (return . AdjList . MM)
    {-# INLINE makeNewM #-}

instance (MutAdjListReqs g k h e l z i w v, M.MakeNewM (AdjList l i w v k e)) =>
    MakeEmptyGraphM (AdjList l i w v k e) where
    makeEmptyGraphM = M.makeNewM
    {-# INLINE makeEmptyGraphM #-}

instance (MutAdjListReqs g k h e l z i w v, AddGraphEdgeM g) =>
    MakeGraphFromEdgesM (AdjList l i w v k e) list

instance (MutAdjListReqs g k h e l z i w v,
    GetGraphNodeCount g,
    ListGraphNodes g,
    Foldable l
    ) => M.UThawM (AdjList l i w v k e) where
    uthawM graph = do
            let (AdjList w) = graph
            mw <- makeNewMM 
            setSizeMM mw (getGraphNodeCount graph)
            mapM_ (\u -> (MP <$> uthawM (w `at` u)) >>= writeMM mw u)
                (listGraphNodes graph)
            return (AdjList (MM mw))
    {-# INLINE uthawM #-}

instance (MutAdjListReqs g k h e l z i w v,
    GetGraphNodeCountC g,
    ListGraphNodesC g,
    MakeNew v (k, e),
    MakeNewM w (v (k, e)),
    UFreezeC w (v (k, e)),
    SetSizeM w (v (k, e)),
    WriteM w (v (k, e)),
    Foldable l,
    MutToCst2 w (v (k, e))
    ) => M.UFreezeC (AdjList l i w v k e) where
    ufreezeC graph = do
            let (AdjList (MM mw)) = graph
            x <- makeNewM
            getGraphNodeCountC graph >>= setSizeM x
            listGraphNodesC graph >>=
                mapM_ (\u -> do
                    MP mv <- readCC mw u
                    ufreezeC mv >>= writeM x u
                    )
            w <- ufreezeC (c2 x)
            return (AdjList w)
    {-# INLINE ufreezeC #-}

class MakeGraphFromEdgesST m g list where 
    makeGraphFromEdgesST :: 
        (MutMonad s m, GraphReqs g k h e l z) => list (k, k, e) -> m g
instance (
    GraphReqs g k h e l z,
    MutMonad s m,
    Foldable list,
    MakeGraphFromEdgesM g list,
    M.UFreezeC g,
    MutToCst g
    ) => MakeGraphFromEdgesST m g list where
    makeGraphFromEdgesST edges = makeGraphFromEdgesM edges >>= M.ufreezeC . cst
    {-# INLINE makeGraphFromEdgesST #-}

instance (forall s . MakeGraphFromEdgesST (ST s) (AdjList l i w v k e) list) =>
    MakeGraphFromEdges (AdjList l i w v k e) list where
    makeGraphFromEdges edges = runST $ makeGraphFromEdgesST edges
    {-# INLINE makeGraphFromEdges #-}


newtype SizeViaNodes g = SizeViaNodes g
type instance Mut s (SizeViaNodes g) = SizeViaNodes (Mut s g)
type instance Cst s (SizeViaNodes g) = SizeViaNodes (Cst s g)
type instance SizeOf (SizeViaNodes (AdjList l i w v k e)) =
    VertexKeyOf (AdjList l i w v k e)

instance (MutAdjListReqs g k h e l z i w v, 
    GetGraphNodeCountC g
    ) => M.GetSizeC (SizeViaNodes (AdjList l i w v k e)) where
    getSizeC (SizeViaNodes g) = getGraphNodeCountC g
    {-# INLINE getSizeC #-}

instance (MutAdjListReqs g k h e l z i w v,
    Foldable l,
    MutToCst (SizeViaNodes g)
    ) => M.GrowSizeM (SizeViaNodes (AdjList l i w v k e)) where
    growSizeM graph addedCount = do
        let (SizeViaNodes (AdjList (MM mw))) = graph
        n <- M.getSizeC (cst graph)
        let l :: l z = enumFromTo n (n + addedCount - 1)
        growSizeMM mw addedCount
        mapM_ (\k -> makeNewM >>= \v -> writeMM mw k (MP v)) l
    {-# INLINE growSizeM #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    M.ShrinkSizeM (SizeViaNodes (AdjList l i w v k e)) where
    shrinkSizeM (SizeViaNodes (AdjList (MM mw))) = shrinkSizeMM mw
    {-# INLINE shrinkSizeM #-}

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    Foldable l,
    MutToCst (SizeViaNodes g)
    ) => M.ModifySizeM (SizeViaNodes (AdjList l i w v k e))

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    Foldable l,
    MutToCst (SizeViaNodes g)
    ) => M.EnsureSizeM (SizeViaNodes (AdjList l i w v k e))
