module MutGraph.AdjacencyList (
    AdjList,
    AdjLst,
) where
import Containers.Prelude
import Control.Monad.ST
import Control.DeepSeq
import Containers.List
import MutGraph.Graph
import MutContainers.Container
import MutContainers.Map
import Containers.Container
import MutState.State

type AdjLst 
    (l :: * -> *) (i :: *) (w :: * -> * -> *) (v :: * -> * -> *) (k :: *) (e :: *) = 
        AdjList l i (w i (v k (k, e))) (v k (k, e)) k e

newtype AdjList 
    (l :: * -> *) (i :: *) (w :: *) (v :: *) (k :: *) (e :: *) = 
    AdjList w 
type instance VertexKeyOf (AdjList l i w v k e) = k
type instance EdgeKeyOf (AdjList l i w v k e) = 
    (VertexKeyOf (AdjList l i w v k e), i)
type instance EdgeValueOf (AdjList l i w v k e) = e
type instance GraphListOf (AdjList l i w v k e) = l
type instance GraphSizeOf (AdjList l i w v k e) = k

type instance Mut s (AdjList l i w v k e) = 
    AdjList l i (Mut s w) (Mut s v) k e
type instance Cst s (AdjList l i w v k e) = 
    AdjList l i (Cst s w) (Cst s v) k e

type AdjListReqs g k h e l z i w v = (
    g ~ AdjList l i w v k e,
    GraphReqs g k h e l z,
    KeyOf v ~ i,
    ValOf v ~ (k, e),
    SizeOf v ~ i,
    KeyOf w ~ k,
    ValOf w ~ v,
    SizeOf w ~ z,
    GetSize w,
    ReadAt w,
    ReadAt v,
    GetSize v,
    Convert v (l (k, e)),
    Convert (l (k, e)) v,
    Num k,
    Enum k,
    Num i,
    Enum i,
    h ~ (k, i),
    ZipWith l,
    EnumFromTo l
    )

type MutAdjListReqs g k h e l z i w v = (
    AdjListReqs g k h e l z i w v,
    ReadC v,
    WriteM v,
    GetSizeC v,
    UFreezeC v,
    UThawM v,
    NewM v,
    GetSizeC w,
    ReadCC w,
    ReadMM w,
    WriteMM w,
    UFreezeC w,
    UThawM w,
    NewM w
    )

instance (NFData w) => NFData (AdjList l i w v k e) where
  rnf (AdjList vle) = rnf vle
  {-# INLINE rnf #-}

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

instance (AdjListReqs g k h e l z i w v) =>
    ListGraphEdgesFrom (AdjList l i w v k e) where
    listGraphEdgesFrom (AdjList w) u = zipWith f lv lh where
        f (k, e) h = Edge (u, k, e, h)
        lv = convert v
        v = w `at` u
        lh = fmap (u,) ln
        ln = enumFromTo 0 n
        n = getSize v - 1
    {-# INLINE listGraphEdgesFrom #-}

instance (AdjListReqs g k h e l z i w v) => 
    ListGraphEdges (AdjList l i w v k e)

instance (MutAdjListReqs g k h e l z i w v) =>
    GetGraphNodeCountC (AdjList l i w v k e) where
    getGraphNodeCountC (AdjList w) = getSizeC w
    {-# INLINE getGraphNodeCountC #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    ReadGraphEdgeC (AdjList l i w v k e) where
    readGraphEdgeC (AdjList w) h = do
        let (u, i) = h
        v <- readCC w u
        (k, e) <- readC v i
        return (Edge (u, k, e, h))
    {-# INLINE readGraphEdgeC #-}

instance (MutAdjListReqs g k h e l z i w v, MutToCst v) =>
    WriteGraphEdgeM (AdjList l i w v k e) where
    writeGraphEdgeM (AdjList w) (u, i) e = readMM w u >>= 
        \v -> modifyM v i (\(k, _) -> (k, e))
    {-# INLINE writeGraphEdgeM #-}

instance (MutAdjListReqs g k h e l z i w v, GetGraphNodeCountC g) => 
    ListGraphNodesC (AdjList l i w v k e) where
    listGraphNodesC g = do
        n <- getGraphNodeCountC g
        return (enumFromTo 0 (n - 1))
    {-# INLINE listGraphNodesC #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    ListGraphEdgesFromC (AdjList l i w v k e) where
    listGraphEdgesFromC (AdjList w) u = do
        mv <- readCC w u
        v <- ufreezeC mv
        return $ getListFromVector v where
            getListFromVector v = zipWith f lv lh where
                f (k, e) h = Edge (u, k, e, h)
                n = getSize v - 1
                lv = convert v
                ln = enumFromTo 0 n
                lh = fmap (u,) ln
    {-# INLINE listGraphEdgesFromC #-}

instance (MutAdjListReqs g k h e l z i w v) => 
    ListGraphEdgesC (AdjList l i w v k e)

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    EnsureSizeM (SizeViaNodes g),
    Snoc l,
    Num (SizeOf l),
    MutToCst w,
    MutToCst v
    ) => AddGraphEdgeM (AdjList l i w v k e) where
    addGraphEdgeM graph (u, u', e) = do
        ensureSizeM (SizeViaNodes graph) (u' + 1)
        let (AdjList mw) = graph
        mv <- readMM mw u
        h <- (u,) <$> getSizeC (cst mv)
        v <- ufreezeC (cst mv)
        mvNew <- uthawM (snoc1 v (u', e))
        writeMM mw u mvNew
        return h
        where
        snoc1 v x = v3
            where
            v3 = convert v2
            v2 :: l (k, e) = snoc v1 x
            v1 = convert v
    {-# INLINE addGraphEdgeM #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    NewM (AdjList l i w v k e) where
    newM = AdjList <$> newM
    {-# INLINE newM #-}

instance (MutAdjListReqs g k h e l z i w v, AddGraphEdgeM g) =>
    MakeGraphFromEdgesM (AdjList l i w v k e) list

instance (MutAdjListReqs g k h e l z i w v) => UThawM (AdjList l i w v k e) where
    uthawM (AdjList w) = AdjList <$> uthawM w
    {-# INLINE uthawM #-}

instance (MutAdjListReqs g k h e l z i w v
    ) => UFreezeC (AdjList l i w v k e) where
    ufreezeC (AdjList mw) = AdjList <$> ufreezeC mw
    {-# INLINE ufreezeC #-}

class MakeGraphFromEdgesST m g list where 
    makeGraphFromEdgesST :: 
        (MutMonad s m, GraphReqs g k h e l z) => list (k, k, e) -> m g
instance (
    GraphReqs g k h e l z,
    MutMonad s m,
    Foldable list,
    MakeGraphFromEdgesM g list,
    UFreezeC g,
    MutToCst g
    ) => MakeGraphFromEdgesST m g list where
    makeGraphFromEdgesST edges = makeGraphFromEdgesM edges >>= ufreezeC . cst
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
    ) => GetSizeC (SizeViaNodes (AdjList l i w v k e)) where
    getSizeC (SizeViaNodes g) = getGraphNodeCountC g
    {-# INLINE getSizeC #-}

instance (MutAdjListReqs g k h e l z i w v,
    GrowSizeM w,
    MutToCst (SizeViaNodes g)
    ) => GrowSizeM (SizeViaNodes (AdjList l i w v k e)) where
    growSizeM graph addedCount = do
        let (SizeViaNodes (AdjList mw)) = graph
        n <- getSizeC (cst graph)
        let l :: l z = enumFromTo n (n + addedCount - 1)
        growSizeM mw addedCount
        mapM_ (\k -> newM >>= \v -> writeMM mw k v) l
    {-# INLINE growSizeM #-}

instance (MutAdjListReqs g k h e l z i w v, ShrinkSizeM w) =>
    ShrinkSizeM (SizeViaNodes (AdjList l i w v k e)) where
    shrinkSizeM (SizeViaNodes (AdjList mw)) = shrinkSizeM mw
    {-# INLINE shrinkSizeM #-}

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    ShrinkSizeM w,
    GrowSizeM w,
    MutToCst (SizeViaNodes g)
    ) => ModifySizeM (SizeViaNodes (AdjList l i w v k e))

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    GrowSizeM w,
    MutToCst (SizeViaNodes g)
    ) => EnsureSizeM (SizeViaNodes (AdjList l i w v k e))
