module MutContainers.Bi.Heap (
    Heap(..),
)
where
import Prelude (Bounded(..), Num(..), Ord(..), Integral(..), Eq(..), ($), (<$>), const)
import Data.Bool
import Control.Monad
import MutContainers.Bi.PriorityQueue
import MutContainers.Bi.Map
import MutContainers.Bi.Container
import MutContainers.Bi.Size
import MutState.State

type HeapReqs v k z a = (
    z ~ k,
    k ~ KeyOf v,
    WriteM v a, ReadC v a,
    Num k, Ord k, Integral k
    )

newtype Heap v z a = Heap (v a, z)
type instance Mut s (Heap v z) = Heap (MutSP (Mut s v)) (MutVar s z)
type instance Cst s (Heap v z) = Heap (MutSP (Cst s v)) (MutVar s z)


type instance SizeOf (Heap v z) = KeyOf v 
type instance KeyOf (Heap v z) = KeyOf v

instance (HeapReqs v k z a) => GetSizeC (Heap v z) a where
    getSizeC (Heap (_, z)) = readMutVar z
    {-# INLINE getSizeC #-}
instance (HeapReqs v k z a) => ModifySizeM (Heap v z) a where
    modifySizeM (Heap (_, z)) = modifyMutVar z
    {-# INLINE modifySizeM #-}

instance (HeapReqs v k z a) => WriteM (Heap v z) a where
    writeM (Heap (MutSP v, _)) = writeM v
    {-# INLINE writeM #-}
instance (HeapReqs v k z a) => ReadC (Heap v z) a where
    readC (Heap (MutSP v, _)) = readC v
    {-# INLINE readC #-}
swapM :: forall l a k s m. (MutMonad s m, 
    k ~ KeyOf l, ReadC l a, WriteM l a, MutToCstC l a) =>
    Mut s l a -> k -> k -> m ()
swapM v i j = do
                x <- readC (cstC v) i
                y <- readC (cstC v) j
                writeM v i y
                writeM v j x
{-# INLINE swapM #-}

instance (HeapReqs v k z a) => IsEmptyC (Heap v z) a where
    isEmptyC heap = (== 0) <$> getSizeC heap
    {-# INLINE isEmptyC #-}

instance (HeapReqs v k z a) => EmptyM (Heap v z) a where
    emptyM heap = modifySizeM heap (const 0)
    {-# INLINE emptyM #-}


instance (HeapReqs v k z a, MutToCstC (Heap v z) a) => InsertValM (Heap v z) a where
    insertValM heap value = do
        prevHeapSize <- getSizeC (cstC heap)
        modifySizeM heap (+1)
        writeM heap prevHeapSize value
        fixHeapProperty heap prevHeapSize
    {-# INLINE insertValM #-}

instance (HeapReqs v k z a, Bounded a, MutToCstC (Heap v z) a) => ExtractMinM (Heap v z) a where
    extractMinM heap = do
        heapSize <- getSizeC (cstC heap)
        rootValue <- readC (cstC heap) 0
        readC (cstC heap) (heapSize - 1) >>= writeM heap 0
        modifySizeM heap (+(-1))
        minHeapify heap 0
        return rootValue
    {-# INLINE extractMinM #-}

-- assertLegalIndex :: (MutMonad s m, l ~ Heap v z, GetSizeC l a, Num (SizeOf l), Ord (SizeOf l)) => Mut s l a -> SizeOf l -> m Bool
-- assertLegalIndex heap index = 
--     assert (index >= 0)
--     $ do
--     s <- getSizeC heap
--     assert (index < s)
--     $ return True
-- {-# INLINE assertLegalIndex #-}

parentKey :: Integral a => a -> a
parentKey i = div (i - 1) 2
{-# INLINE parentKey #-}
leftKey :: Num a => a -> a
leftKey i = 2 * i + 1
{-# INLINE leftKey #-}
rightKey :: Num a => a -> a
rightKey i = 2 * i + 2
{-# INLINE rightKey #-}

fixHeapProperty :: forall l a k s m. (MutMonad s m, 
    k ~ KeyOf l, Integral k,
    ReadC l a, WriteM l a, Ord a, MutToCstC l a) =>
    Mut s l a -> k -> m ()
fixHeapProperty h _k =
    readC (cstC h) _k >>= loop _k
    where 
        loop k v = unless (k == 0) $ do
                let pk = parentKey k
                pv <- readC (cstC h) pk
                unless (pv <= v) $ do
                    swapM h pk k
                    loop pk v
{-# INLINE fixHeapProperty #-}

minHeapify :: forall l a k s m. (MutMonad s m, GetSizeC l a,
    k ~ KeyOf l, k ~ SizeOf l, Num k, Ord k,
    ReadC l a, WriteM l a, Bounded a, Ord a, MutToCstC l a) =>
    Mut s l a -> k -> m ()
minHeapify h _k = do
    n <- getSizeC (cstC h)
    hk <- readC (cstC h) _k
    preloop n hk _k
    where
    preloop n hk = loop
        where
        loop k = do
            let l = leftKey k
            let r = rightKey k
            hl <- if l < n then readC (cstC h) l else return maxBound
            hr <- if r < n then readC (cstC h) r else return maxBound
            if hl < hr && hl < hk then swap l
            else when (hr < hl && hr < hk) $ swap r
            where
            swap j = do
                swapM h k j
                loop j
{-# INLINE minHeapify #-}
