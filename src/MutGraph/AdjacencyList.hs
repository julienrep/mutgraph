module MutGraph.AdjacencyList (
    AdjList,
    AdjLst,
) where
import Prelude (Num(..), Enum, (.), (<$>), ($))
import Data.Foldable hiding (concat)
import Data.Traversable
import Data.Ord
import Control.Monad
import Control.Monad.ST
import Control.DeepSeq
import MutGraph.Graph
import MutContainers.Mono.Container
import MutContainers.Mono.Map
import MutContainers.Mono.Size
import MutContainers.Mo.List
import MutState.State
import MutContainers.Any.Map
import MutContainers.Any.Size

type AdjLst 
    (l :: * -> *) (i :: *) (w :: * -> *) (v :: * -> *) (k :: *) (e :: *) = 
        AdjList l i (w (v (k, e))) (v (k, e)) k e

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
    KeyOf w ~ k,
    ValOf w ~ v,
    SizeOf w ~ z,
    KeyOf v ~ i,
    ValOf v ~ (k, e),
    SizeOf v ~ i,
    GetSize w,
    ReadAt w,
    ReadAt v,
    Num k,
    Enum k,
    EnumFromTo l,
    h ~ (k, i),
    Num i,
    GetSize v,
    Convert v (l (k, e)),
    Convert (l (k, e)) v,
    Map l,
    Zip l,
    Enum i
    )

type MutAdjListReqs g k h e l z i w v = (
    AdjListReqs g k h e l z i w v,
    GetSizeC w,
    ReadCC w,
    ReadMM w,
    ReadC v,
    WriteM v,
    WriteMM w,
    GetSizeC v,
    GrowSizeM w,
    ShrinkSizeM w,
    SetSizeM w,
    UFreezeC v,
    UFreezeC w,
    UThawM v,
    MakeNewM v,
    MakeNewM w
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
    listGraphEdgesFrom (AdjList w) u =
            map (\((k, e), h) -> Edge (u, k, e, h))
            (zip lv (map (u,) (enumFromTo 0 n)))
        where
        v = w `at` u
        n = getSize v - 1
        lv = convert v
    {-# INLINE listGraphEdgesFrom #-}

instance (AdjListReqs g k h e l z i w v, Monad l) => 
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
        let n = getSize v - 1
        let lv = convert v
        return  (map (\((k, e), h) -> Edge (u, k, e, h))
                (zip lv (map (u,) (enumFromTo 0 n))))
    {-# INLINE listGraphEdgesFromC #-}

instance (MutAdjListReqs g k h e l z i w v, Traversable l, Monad l) => 
    ListGraphEdgesC (AdjList l i w v k e)

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    EnsureSizeM (SizeViaNodes g),
    Concat l,
    Replicate l,
    SizeOf l ~ k,
    MutToCst w,
    MutToCst v
    ) => AddGraphEdgeM (AdjList l i w v k e) where
    addGraphEdgeM graph (u, u', e) = do
        ensureSizeM (SizeViaNodes graph) (u' + 1)
        let (AdjList w) = graph
        v <- readMM w u
        h <- (u,) <$> getSizeC (cst v)
        cv <- ufreezeC (cst v)
        nv <- uthawM (addNewEdge cv)
        writeMM w u nv
        return h
        where
            addNewEdge v = convert v2
                where
                    v2 = concat v1 (replicate 1 (u', e))
                    v1 = convert v :: l (k, e)
    {-# INLINE addGraphEdgeM #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    MakeNewM (AdjList l i w v k e) where
    makeNewM = makeNewM >>= (return . AdjList)
    {-# INLINE makeNewM #-}

instance (MutAdjListReqs g k h e l z i w v, MakeNewM (AdjList l i w v k e)) =>
    MakeEmptyGraphM (AdjList l i w v k e) where
    makeEmptyGraphM = makeNewM
    {-# INLINE makeEmptyGraphM #-}

instance (MutAdjListReqs g k h e l z i w v, AddGraphEdgeM g) =>
    MakeGraphFromEdgesM (AdjList l i w v k e) list

instance (MutAdjListReqs g k h e l z i w v,
    UThawM w
    ) => UThawM (AdjList l i w v k e) where
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
    Foldable l,
    MutToCst (SizeViaNodes g)
    ) => GrowSizeM (SizeViaNodes (AdjList l i w v k e)) where
    growSizeM graph addedCount = do
        let (SizeViaNodes (AdjList mw)) = graph
        n <- getSizeC (cst graph)
        let l :: l z = enumFromTo n (n + addedCount - 1)
        growSizeM mw addedCount
        mapM_ (\k -> makeNewM >>= \v -> writeMM mw k v) l
    {-# INLINE growSizeM #-}

instance (MutAdjListReqs g k h e l z i w v) =>
    ShrinkSizeM (SizeViaNodes (AdjList l i w v k e)) where
    shrinkSizeM (SizeViaNodes (AdjList mw)) = shrinkSizeM mw
    {-# INLINE shrinkSizeM #-}

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    Foldable l,
    MutToCst (SizeViaNodes g)
    ) => ModifySizeM (SizeViaNodes (AdjList l i w v k e))

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    Foldable l,
    MutToCst (SizeViaNodes g)
    ) => EnsureSizeM (SizeViaNodes (AdjList l i w v k e))
