module MutContainers.Tri.Heap (
    Heap, MakeHeapM(..),
)
where
import Prelude (Bounded(..), Num(..), Ord(..), Integral(..), Eq(..), ($), (<$>), const)
import Data.Bool
import Control.Monad
import MutContainers.Bi.PriorityQueue
import MutContainers.Tri.Map
import MutContainers.Bi.Container
import MutContainers.Mono.Size
import MutState.State

newtype Heap (h :: * -> * -> *) (z :: *) (k :: *) (a :: *) = Heap (h k a, z)
type instance Mut s (Heap h z) = Heap ( (Mut s h)) (Mut s (MutV z))
type instance Cst s (Heap h z) = Heap ( (Cst s h)) (Cst s (MutV z))
type instance Mut s (Heap h z k) = Heap ( (Mut s h)) (Mut s (MutV z)) k
type instance Cst s (Heap h z k) = Heap ( (Cst s h)) (Cst s (MutV z)) k
type instance Mut s (Heap h z k a) = Heap ( (Mut s h)) (Mut s (MutV z)) k a
type instance Cst s (Heap h z k a) = Heap ( (Cst s h)) (Cst s (MutV z)) k a

class MakeHeapM (h :: * -> * -> *) (q :: * -> * -> *) (z :: *) (k :: *) (a :: *) where
    makeHeapM :: (MutMonad s m) => Mut s h k a -> z -> m (Mut s q k a)
instance MakeHeapM h (Heap h z) z k a where
    makeHeapM h z = newMutV z >>= \zVar -> return (Heap (h, zVar))

type instance SizeOf (Heap h z k a) = z
instance () => GetSizeC (Heap h z k a) where
    getSizeC (Heap (_, z)) = readMutV z
    {-# INLINE getSizeC #-}
instance () => ModifySizeM (Heap h z k a) where
    modifySizeM (Heap (_, z)) = modifyMutV z
    {-# INLINE modifySizeM #-}

instance (WriteM h a) => WriteM (Heap h z) a where
    writeM (Heap (h, _)) = writeM h
    {-# INLINE writeM #-}
instance (ReadC h a) => ReadC (Heap h z) a where
    readC (Heap (h, _)) = readC h
    {-# INLINE readC #-}
swapM :: forall h a k s m. (MutMonad s m, 
    ReadC h a, WriteM h a, MutToCst3 h k a) =>
    Mut s h k a -> k -> k -> m ()
swapM h i j = do
                x <- readC (c3 h) i
                y <- readC (c3 h) j
                writeM h i y
                writeM h j x
{-# INLINE swapM #-}

instance (Num z, Ord z) => IsEmptyC (Heap h z k) a where
    isEmptyC heap = (== 0) <$> getSizeC heap
    {-# INLINE isEmptyC #-}

instance (Num z) => EmptyM (Heap h z k) a where
    emptyM heap = modifySizeM heap (const 0)
    {-# INLINE emptyM #-}

instance (
    z ~ k,
    Num z,
    q ~ Heap h z,
    WriteM q a, ReadC q a,
    FixHeapProperty q k a,
    MutToCst (q k a)
    ) => InsertValM (Heap h z k) a where
    insertValM heap value = do
        prevHeapSize <- getSizeC (cst heap)
        modifySizeM heap (+1)
        writeM heap prevHeapSize value
        fixHeapProperty heap prevHeapSize
    {-# INLINE insertValM #-}

instance (
    z ~ k,
    Num z,
    q ~ Heap h z,
    WriteM q a, ReadC q a,
    MinHeapify q k a,
    MutToCst (q k a)
    ) => ExtractMinM (Heap h z k) a where
    extractMinM heap = do
        heapSize <- getSizeC (cst heap)
        rootValue <- readC (cst heap) 0
        readC (cst heap) (heapSize - 1) >>= writeM heap 0
        modifySizeM heap (+(-1))
        minHeapify heap 0
        return rootValue
    {-# INLINE extractMinM #-}

parentKey :: Integral a => a -> a
parentKey i = div (i - 1) 2
{-# INLINE parentKey #-}
leftKey :: Num a => a -> a
leftKey i = 2 * i + 1
{-# INLINE leftKey #-}
rightKey :: Num a => a -> a
rightKey i = 2 * i + 2
{-# INLINE rightKey #-}

class FixHeapProperty (q :: * -> * -> *) (k :: *) (a :: *) where
    fixHeapProperty :: (MutMonad s m, ReadC q a, WriteM q a) =>
        Mut s q k a -> k -> m ()
instance (
    Integral k,
    Ord a,
    MutToCst3 q k a
    ) => FixHeapProperty q k a where
    fixHeapProperty heap _k =
        readC (c3 heap) _k >>= loop _k
        where 
            loop k v = unless (k == 0) $ do
                    let pk = parentKey k
                    pv <- readC (c3 heap) pk
                    unless (pv <= v) $ do
                        swapM heap pk k
                        loop pk v
    {-# INLINE fixHeapProperty #-}


class MinHeapify (q :: * -> * -> *) (k :: *) (a :: *) where
    minHeapify :: (MutMonad s m, ReadC q a, WriteM q a) => 
        Mut s q k a -> k -> m ()
instance (
    Bounded a, Ord a,
    Num k, Ord k,
    k ~ SizeOf (q k a),
    GetSizeC (q k a),
    MutToCst3 q k a
    ) => MinHeapify q k a where
    minHeapify heap _k = do
        n <- getSizeC (c1m3 heap)
        hk <- readC (c3 heap) _k
        preloop n hk _k
        where
        preloop n hk = loop
            where
            loop k = do
                let l = leftKey k
                let r = rightKey k
                hl <- if l < n then readC (c3 heap) l else return maxBound
                hr <- if r < n then readC (c3 heap) r else return maxBound
                if hl < hr && hl < hk then swap l
                else when (hr < hl && hr < hk) $ swap r
                where
                swap j = do
                    swapM heap k j
                    loop j
    {-# INLINE minHeapify #-}


-- -- assertLegalIndex :: (MutMonad s m, q ~ Heap h z, GetSizeC q a, Num (SizeOf q), Ord (SizeOf q)) => Mut s q a -> SizeOf q -> m Bool
-- -- assertLegalIndex heap index = 
-- --     assert (index >= 0)
-- --     $ do
-- --     s <- getSizeC heap
-- --     assert (index < s)
-- --     $ return True
-- -- {-# INLINE assertLegalIndex #-}