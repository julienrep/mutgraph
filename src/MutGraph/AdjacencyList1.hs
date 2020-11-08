module MutGraph.AdjacencyList1 (
    AdjList(..),
) where
import Prelude (Num(..), (.), (<$>), ($))
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
import MutContainers.Mono.List
import qualified MutContainers.Bi.List as B
import MutState.State
import MutContainers.Any.Map

newtype AdjList 
    (l :: * -> *) (i :: *) (w :: *) (v :: *) (k :: *) (e :: *) = 
    AdjList w 
type instance VertexKeyOf (AdjList l i w v k e) = k
type instance EdgeKeyOf (AdjList l i w v k e) = 
    (VertexKeyOf (AdjList l i w v k e), i)
type instance EdgeValueOf (AdjList l i w v k e) = e
type instance GraphListOf (AdjList l i w v k e) = l
type instance GraphSizeOf (AdjList l i w v k e) = k

-- type instance ValOf (AdjList l i w v k e) = w

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
    k ~ ValOf (l k),
    EnumFromTo (l k),
    h ~ (k, i),
    Num i,
    GetSize v,
    Convert v (l (k, e)),
    B.Map l ((k, e), h) (Edge g),
    B.Map l i h,
    B.Zip l (k, e) h,
    B.EnumFromTo l i
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

instance (AdjListReqs g k h e l z i w v, GetGraphNodeCount g) =>
    ListGraphEdgesFrom (AdjList l i w v k e) where
    listGraphEdgesFrom (AdjList w) u =
            B.map (\((k, e), h) -> Edge (u, k, e, h))
            (B.zip lv (B.map (u,) (B.enumFromTo 0 n)))
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

instance (MutAdjListReqs g k h e l z i w v, MutToCst v, MutToCst w) =>
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
        return  (B.map (\((k, e), h) -> Edge (u, k, e, h))
                (B.zip lv (B.map (u,) (B.enumFromTo 0 n))))
    {-# INLINE listGraphEdgesFromC #-}

instance (MutAdjListReqs g k h e l z i w v, Traversable l, Monad l) => 
    ListGraphEdgesC (AdjList l i w v k e)

instance (MutAdjListReqs g k h e l z i w v, Ord k,
    EnsureSizeM (SizeViaNodes g),
    Concat v,
    Replicate v,
    MutToCst w,
    MutToCst v
    ) => AddGraphEdgeM (AdjList l i w v k e) where
    addGraphEdgeM graph (u, u', e) = do
        ensureSizeM (SizeViaNodes graph) (u' + 1)
        let (AdjList w) = graph
        v <- readMM w u
        h <- (u,) <$> getSizeC (cst v)
        fv <- ufreezeC (cst v)
        nv <- uthawM (concat fv (replicate 1 (u', e)))
        writeMM w u nv
        return h
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
    GetGraphNodeCount g,
    ListGraphNodes g,
    Foldable l
    ) => UThawM (AdjList l i w v k e) where
    uthawM graph = do
            let (AdjList w) = graph
            mw <- makeNewM 
            setSizeM mw (getGraphNodeCount graph)
            mapM_ (\u -> uthawM (w `at` u) >>= writeMM mw u)
                (listGraphNodes graph)
            return (AdjList mw)
    {-# INLINE uthawM #-}

instance (MutAdjListReqs g k h e l z i w v,
    GetGraphNodeCountC g,
    ListGraphNodesC g,
    WriteM w,
    Foldable l,
    MutToCst w
    ) => UFreezeC (AdjList l i w v k e) where
    ufreezeC graph = do
            let (AdjList mw) = graph
            x <- makeNewM
            getGraphNodeCountC graph >>= setSizeM x
            listGraphNodesC graph >>=
                mapM_ (\u -> do
                    mv <- readCC mw u
                    ufreezeC mv >>= writeM x u
                    )
            w <- ufreezeC (cst x)
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
