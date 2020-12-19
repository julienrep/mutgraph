module MutContainers.Mono.Heap (
    Heap, MakeHeapM(..),
)
where
import Prelude (Bounded(..), Num(..), Ord(..), Integral(..), Eq(..), ($), (<$>), const)
import Data.Bool
import Control.Monad
import MutContainers.Mono.PriorityQueue
import MutContainers.Mono.Map
import MutContainers.Mono.Container
import MutContainers.Mono.Size
import MutState.State
import MutContainers.Any.Map
import MutContainers.Any.Size

newtype Heap h z = Heap (h, z)
type instance Mut s (Heap h z) = Heap ( (Mut s h)) (Mut s (Var z))
type instance Cst s (Heap h z) = Heap ( (Cst s h)) (Cst s (Var z))

type instance KeyOf (Heap h z) = KeyOf h
type instance ValOf (Heap h z) = ValOf h
type instance SizeOf (Heap h z) = z

class MakeHeapM h q z a where
    makeHeapM :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Mut s h -> z -> m (Mut s q)
instance MakeHeapM h (Heap h z) z a where
    makeHeapM h z = newMutV z >>= \zVar -> return (Heap (h, zVar))

type instance SizeOf (Heap h z) = z
instance () => GetSizeC (Heap h z) where
    getSizeC (Heap (_, z)) = readMutV z
    {-# INLINE getSizeC #-}
instance () => ModifySizeM (Heap h z) where
    modifySizeM (Heap (_, z)) = modifyMutV z
    {-# INLINE modifySizeM #-}

instance (WriteM h) => WriteM (Heap h z) where
    writeM (Heap (h, _)) = writeM h
    {-# INLINE writeM #-}
instance (ReadC h) => ReadC (Heap h z) where
    readC (Heap (h, _)) = readC h
    {-# INLINE readC #-}
swapM :: forall h k s m. (MutMonad s m, k ~ KeyOf h,
    ReadC h, WriteM h, MutToCst h) =>
    Mut s h -> k -> k -> m ()
swapM h i j = do
                x <- readC (cst h) i
                y <- readC (cst h) j
                writeM h i y
                writeM h j x
{-# INLINE swapM #-}

instance (Num z, Ord z) => IsEmptyC (Heap h z) where
    isEmptyC heap = (== 0) <$> getSizeC heap
    {-# INLINE isEmptyC #-}

instance (Num z) => EmptyM (Heap h z) where
    emptyM heap = modifySizeM heap (const 0)
    {-# INLINE emptyM #-}

instance (
    z ~ k,
    Num z,
    q ~ Heap h z,
    k ~ KeyOf q,
    WriteM q, ReadC q,
    FixHeapProperty q,
    MutToCst q
    ) => InsertValM (Heap h z) where
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
    k ~ KeyOf q,
    WriteM q, ReadC q,
    MinHeapify q,
    MutToCst q
    ) => ExtractMinM (Heap h z) where
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

class FixHeapProperty q where
    fixHeapProperty :: (MutMonad s m, ReadC q, WriteM q, k ~ KeyOf q) =>
        Mut s q -> k -> m ()
instance (
    k ~ KeyOf q,
    a ~ ValOf q,
    Integral k,
    Ord a,
    MutToCst q
    ) => FixHeapProperty q where
    fixHeapProperty heap _k =
        readC (cst heap) _k >>= loop _k
        where 
            loop k v = unless (k == 0) $ do
                    let pk = parentKey k
                    pv <- readC (cst heap) pk
                    unless (pv <= v) $ do
                        swapM heap pk k
                        loop pk v
    {-# INLINE fixHeapProperty #-}

class MinHeapify q where
    minHeapify :: (MutMonad s m, ReadC q, WriteM q, k ~ KeyOf q) => 
        Mut s q -> k -> m ()
instance (
    k ~ KeyOf q,
    a ~ ValOf q,
    k ~ SizeOf q,
    Bounded a, Ord a,
    Num k, Ord k,
    GetSizeC q,
    MutToCst q
    ) => MinHeapify q where
    minHeapify heap _k = do
        n <- getSizeC (cst heap)
        hk <- readC (cst heap) _k
        preloop n hk _k
        where
        preloop n hk = loop
            where
            loop k = do
                let l = leftKey k
                let r = rightKey k
                hl <- if l < n then readC (cst heap) l else return maxBound
                hr <- if r < n then readC (cst heap) r else return maxBound
                if hl < hr && hl < hk then swap l
                else when (hr < hl && hr < hk) $ swap r
                where
                swap j = do
                    swapM heap k j
                    loop j
    {-# INLINE minHeapify #-}
