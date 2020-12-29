module MutContainers.Heap (
    Heap(..), MakeHeapM(..),
)
where
import Prelude (Bounded(..), Num(..), Ord(..), Integral(..), Eq(..), ($), (<$>), const)
import Data.Bool
import Control.Monad
import MutContainers.PriorityQueue
import MutContainers.Map
import MutContainers.Container
import Containers.Container (SizeOf)
import MutState.State

newtype Heap h = Heap h
type instance Mut s (Heap h) = (Mut s h, Mut s (Var (SizeOf (Heap h))))
type instance Cst s (Heap h) = (Cst s h, Cst s (Var (SizeOf (Heap h))))
type instance KeyOf (Heap h) = KeyOf h
type instance ValOf (Heap h) = ValOf h
type instance SizeOf (Heap h) = KeyOf (Heap h)

class MakeHeapM h where
    makeHeapM :: (MutMonad s m, z ~ SizeOf (Heap h)) =>
        Mut s h -> z -> m (Mut s (Heap h))
instance MakeHeapM h where
    makeHeapM h z = newVar z >>= \zVar -> return (h, zVar)
    {-# INLINE makeHeapM #-}

instance () => GetSizeC (Heap h) where
    getSizeC (_, z) = readVar z
    {-# INLINE getSizeC #-}
instance () => ModifySizeM (Heap h) where
    modifySizeM (_, z) = modifyVar z
    {-# INLINE modifySizeM #-}

instance (WriteM h) => WriteM (Heap h) where
    writeM (h, _) = writeM h
    {-# INLINE writeM #-}
instance (ReadC h) => ReadC (Heap h) where
    readC (h, _) = readC h
    {-# INLINE readC #-}
swapM :: forall q k s m. (MutMonad s m, k ~ KeyOf q,
    ReadC q, WriteM q, MutToCst q) =>
    Mut s q -> k -> k -> m ()
swapM h i j = do
                x <- readC (cst h) i
                y <- readC (cst h) j
                writeM h i y
                writeM h j x
{-# INLINE swapM #-}

instance (z ~ SizeOf (Heap h), Num z, Ord z) => IsEmptyC (Heap h) where
    isEmptyC heap = (== 0) <$> getSizeC heap
    {-# INLINE isEmptyC #-}

instance (z ~ SizeOf (Heap h), Num z) => EmptyM (Heap h) where
    emptyM heap = modifySizeM heap (const 0)
    {-# INLINE emptyM #-}

instance (
    q ~ Heap h,
    k ~ KeyOf q,
    WriteM q, ReadC q,
    Integral k,
    MutToCst q
    ) => InsertValM (Heap h) where
    insertValM heap value = do
        prevHeapSize <- getSizeC (cst heap)
        modifySizeM heap (+1)
        writeM heap prevHeapSize value
        fixHeapProperty heap prevHeapSize
    {-# INLINE insertValM #-}

instance (
    q ~ Heap h,
    k ~ KeyOf q,
    Num k, Ord k,
    WriteM q, ReadC q,
    a ~ ValOf q,
    Bounded a, Ord a,
    MutToCst q
    ) => ExtractMinM (Heap h) where
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

fixHeapProperty :: forall q k a s m. (
    MutMonad s m,
    ReadC q, WriteM q,
    k ~ KeyOf q,
    a ~ ValOf q,
    Integral k,
    Ord a,
    MutToCst q
    ) => Mut s q -> k -> m ()
fixHeapProperty heap _k =
    readC (cst heap) _k >>= loop _k where 
        loop k v = unless (k == 0) $ do
                let pk = parentKey k
                pv <- readC (cst heap) pk
                unless (pv <= v) $ do
                    swapM heap pk k
                    loop pk v
{-# INLINE fixHeapProperty #-}

minHeapify :: forall q k a s m.
    (MutMonad s m, 
    ReadC q, WriteM q,
    GetSizeC q,
    k ~ KeyOf q,
    k ~ SizeOf q,
    a ~ ValOf q,
    Bounded a, Ord a,
    Num k, Ord k,
    MutToCst q
    ) => Mut s q -> k -> m ()
minHeapify heap _k = do
    n <- getSizeC (cst heap)
    hk <- readC (cst heap) _k
    preloop n hk _k where
    preloop n hk = loop where
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
