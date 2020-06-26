module MutContainers.Vector (
    Vec, MVec, DVec, MDVec,
    Vector, VectorU, VectorS,
    MVector, MVectorU, MVectorS,
    DVector, DVectorU, DVectorS,
)
where
import Prelude
import Control.Monad
import Control.DeepSeq
import MutState.State
import MutContainers.Bi.Container
import MutContainers.Bi.Map
import MutContainers.Bi.Size
import MutContainers.Bi.List
import qualified MutContainers.Mono.Container as M
import qualified MutContainers.Mono.List as M
import qualified MutContainers.Mono.Size as M
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Generic.Mutable   as VM
import qualified Data.Vector                   as VI
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Storable          as VS
import qualified Data.Vector.Mutable           as VMI
import qualified Data.Vector.Unboxed.Mutable   as VMU
import qualified Data.Vector.Storable.Mutable  as VMS

-- vector library has generic interfaces, however it lacks newtype wrappers for those interfaces
-- this module defines MVec as newtype wrapper for generic mutable vectors
-- this module defines Vec as newtype wrapper for generic vectors

newtype Vec v a = Vec (v a) 
newtype MVec v s a = MVec (v s a)

newtype DVec v a = DVec (v a)
newtype MDVec v s a = MDVec (MutVar s (v s a))

-- type instance Mut s (Vec v a) = MVec (V.Mutable v) s a
type instance Mut s (Vec v) = MVec (V.Mutable v) s
type instance Cst s (Vec v) = MVec (V.Mutable v) s

-- type instance Mut s (DVec v a) = MDVec (V.Mutable v) s a
type instance Mut s (DVec v) = MDVec (V.Mutable v) s
type instance Cst s (DVec v) = MDVec (V.Mutable v) s

type instance V.Mutable (Vec v) = MVec (V.Mutable v)
type instance V.Mutable (DVec v) = MDVec (V.Mutable v)

instance (VM.MVector v a) => VM.MVector (MVec v) a where
    basicLength (MVec mv) = VM.basicLength mv
    {-# inline basicLength #-}
    basicUnsafeSlice i l (MVec mv) = MVec (VM.basicUnsafeSlice i l mv)
    {-# inline basicUnsafeSlice #-}
    basicOverlaps (MVec mv1) (MVec mv2) = VM.basicOverlaps mv1 mv2
    {-# inline basicOverlaps #-}
    basicUnsafeNew n = MVec <$> VM.basicUnsafeNew n
    {-# inline basicUnsafeNew #-}
    basicInitialize (MVec mv) = VM.basicInitialize mv
    {-# inline basicInitialize #-}
    basicUnsafeRead (MVec mv) = VM.basicUnsafeRead mv
    {-# inline basicUnsafeRead #-}
    basicUnsafeWrite (MVec mv) = VM.basicUnsafeWrite mv
    {-# inline basicUnsafeWrite #-}

instance (V.Vector v a) => V.Vector (Vec v) a where
    basicUnsafeFreeze (MVec mv) = Vec <$> V.basicUnsafeFreeze mv
    {-# inline basicUnsafeFreeze #-}
    basicUnsafeThaw (Vec v) = V.basicUnsafeThaw v >>= (return . MVec)
    {-# inline basicUnsafeThaw #-}
    basicLength (Vec v) = V.basicLength v
    {-# inline basicLength #-}
    basicUnsafeSlice i l (Vec v) = Vec (V.basicUnsafeSlice i l v)
    {-# inline basicUnsafeSlice #-}
    basicUnsafeIndexM (Vec v) = V.basicUnsafeIndexM v
    {-# inline basicUnsafeIndexM #-}

type Vector = Vec VI.Vector
type VectorU = Vec VU.Vector
type VectorS = Vec VS.Vector
type MVector = MVec VMI.MVector
type MVectorU = MVec VMU.MVector
type MVectorS = MVec VMS.MVector
type DVector = DVec VI.Vector
type DVectorU = DVec VU.Vector
type DVectorS = DVec VS.Vector

-- inherit typeclasses -- need to find some automated deriving mechanism
instance (NFData (v a)) => NFData (Vec v a) where
  rnf (Vec v) = rnf v
  {-# INLINE rnf #-}
instance NFData (MVec v s a) where
  rnf (MVec _) = ()
  {-# INLINE rnf #-}
instance (NFData (v a)) => NFData (DVec v a) where
  rnf (DVec v) = rnf v
  {-# INLINE rnf #-}
instance NFData (MDVec v s a) where
    rnf (MDVec _) = ()
--   rnf (MDVec v) = runST (readMutVar v >>= \x -> return (rnf x))
    {-# INLINE rnf #-}

instance (Functor v) => Functor (Vec v) where
    fmap f (Vec l) = Vec (fmap f l)
    {-# inline fmap #-}
instance (Foldable v) => Foldable (Vec v) where
    foldMap f (Vec l) = foldMap f l
    {-# inline foldMap #-}
    foldr f z (Vec l) = foldr f z l
    {-# inline foldr #-}
instance (Traversable v) => Traversable (Vec v) where
    traverse f (Vec l) = Vec <$> traverse f l
    {-# inline traverse #-}
    sequenceA (Vec l) = Vec <$> sequenceA l
    {-# inline sequenceA #-}
instance (Applicative v) => Applicative (Vec v) where
    pure x = Vec (pure x)
    {-# inline pure #-}
    (Vec f) <*> (Vec l) = Vec (f <*> l)
    {-# inline (<*>) #-}
instance (Monad v) => Monad (Vec v) where
    (Vec ma) >>= f = Vec (ma >>= \x -> let (Vec v) = f x in v)
    {-# inline (>>=) #-}
instance (Semigroup (v a)) => Semigroup (Vec v a) where
    (Vec v) <> (Vec w) = Vec (v <> w)
    {-# inline (<>) #-}
instance (Monoid (v a)) => Monoid (Vec v a) where
    mempty = Vec mempty


-- Vector

type instance KeyOf (Vec v) = Int
type instance KeyOf (DVec v) = Int
type instance SizeOf (Vec v) = KeyOf (Vec v)
type instance SizeOf (DVec v) = KeyOf (DVec v)
type instance M.SizeOf (Vec v a) = KeyOf (Vec v)
type instance M.SizeOf (DVec v a) = KeyOf (DVec v)

instance (mv ~ V.Mutable v, VM.MVector mv a) => WriteM (Vec v) a where
    writeM = VM.unsafeWrite
    {-# INLINE writeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => WriteM (DVec v) a where
    writeM (MDVec vl) k a = readMutVar vl >>= \l -> VM.unsafeWrite l k a
    {-# INLINE writeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ReadC (Vec v) a where
    readC = VM.unsafeRead
    {-# INLINE readC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ReadC (DVec v) a where
    readC (MDVec vl) k = readMutVar vl >>= \l -> VM.unsafeRead l k
    {-# INLINE readC #-}
instance (V.Vector v a) => ReadAt (Vec v) a where
    at = (V.!)
    {-# INLINE at #-}
instance (V.Vector v a) => ReadAt (DVec v) a where
    (DVec v) `at` u = v V.! u
    {-# INLINE at #-}

instance (V.Vector v a) => FreezeC (Vec v) a where
    freezeC = V.freeze
    {-# INLINE freezeC #-}
instance (V.Vector v a) => FreezeC (DVec v) a where
    freezeC (MDVec vl) = DVec <$> (readMutVar vl >>= V.freeze)
    {-# INLINE freezeC #-}
instance (V.Vector v a) => UFreezeC (Vec v) a where
    ufreezeC = V.unsafeFreeze
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => UFreezeC (DVec v) a where
    ufreezeC (MDVec vl) = DVec <$> (readMutVar vl >>= V.unsafeFreeze)
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => ThawM (Vec v) a where
    thawM = V.thaw
    {-# INLINE thawM #-}
instance (V.Vector v a) => ThawM (DVec v) a where
    thawM (DVec l) = MDVec <$> (V.thaw l >>= newMutVar)
    {-# INLINE thawM #-}
instance (V.Vector v a) => UThawM (Vec v) a where
    uthawM = V.unsafeThaw
    {-# INLINE uthawM #-}
instance (V.Vector v a) => UThawM (DVec v) a where
    uthawM (DVec l) = MDVec <$> (V.unsafeThaw l >>= newMutVar)
    {-# INLINE uthawM #-}

instance (V.Vector v a) => Convert (Vec v) v a where
    convert (Vec v) = v
    {-# INLINE convert #-}
instance (V.Vector v a) => Convert v (Vec v) a where
    convert = Vec
    {-# INLINE convert #-}
instance (V.Vector v a, V.Vector w a) => Convert (Vec v) (Vec w) a where
    convert (Vec v) = Vec (V.convert v)
    {-# INLINE convert #-}

instance (V.Vector v a, V.Vector v b, V.Vector v (a, b)) => Zip (Vec v) a b where
    zip = V.zip
    {-# INLINE zip #-}
instance (V.Vector v a, V.Vector v b) => Map (Vec v) a b where
    map = V.map
    {-# INLINE map #-}
instance (V.Vector v Int) => EnumFromTo (Vec v) Int where
    enumFromTo a b = V.enumFromN a (b - a + 1)
    {-# INLINE enumFromTo #-}
instance (V.Vector v a) => Concat (Vec v) a where
    concat l l' = V.concat [l, l']
    {-# INLINE concat #-}
instance (V.Vector v a) => Replicate (Vec v) a where
    replicate = V.replicate
    {-# INLINE replicate #-}
instance (V.Vector v a) => MakeNew (Vec v) a where
    makeNew = V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => MakeNew (DVec v) a where
    makeNew = DVec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => GetSize (Vec v) a where
    getSize = V.length
    {-# INLINE getSize #-}
instance (V.Vector v a) => GetSize (DVec v) a where
    getSize (DVec v) = V.length v
    {-# INLINE getSize #-}

instance (mv ~ V.Mutable v, VM.MVector mv a) => ReplicateM (Vec v) a where
    replicateM = VM.replicateM
    {-# INLINE replicateM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ReplicateM (DVec v) a where
    replicateM n v = MDVec <$> (VM.replicateM n v >>= newMutVar)
    {-# INLINE replicateM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => MakeNewM (Vec v) a where
    makeNewM = VM.new 0
    {-# INLINE makeNewM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => MakeNewM (DVec v) a where
    makeNewM = MDVec <$> (VM.new 0 >>= newMutVar)
    {-# INLINE makeNewM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => GetSizeC (Vec v) a where
    getSizeC = return . VM.length
    {-# INLINE getSizeC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => GetSizeC (DVec v) a where
    getSizeC (MDVec vl) = readMutVar vl >>= \l -> return (VM.length l)
    {-# INLINE getSizeC #-}

instance (mv ~ V.Mutable v, VM.MVector mv a) => GrowSizeM (DVec v) a where
    growSizeM (MDVec vl) n = readMutVar vl >>= flip VM.unsafeGrow n >>= writeMutVar vl
    {-# INLINE growSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ShrinkSizeM (DVec v) a where
    shrinkSizeM (MDVec vl) n = readMutVar vl >>= \l -> writeMutVar vl (VM.unsafeSlice 0 n l)
    {-# INLINE shrinkSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ModifySizeM (DVec v) a where
    modifySizeM x f = do
        let MDVec vl = x
        size <- readMutVar vl >>= \l -> return (VM.length l)
        let diff = (f size) - size
        if diff > 0 then growSizeM x diff
        else when (diff < 0) $ shrinkSizeM x diff
    {-# INLINE modifySizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => SetSizeM (DVec v) a
instance (mv ~ V.Mutable v, VM.MVector mv a) => EnsureSizeM (DVec v) a where
    ensureSizeM x z = do
        let MDVec vl = x
        size <- readMutVar vl >>= \l -> return (VM.length l)
        let diff = z - size
        when (diff > 0) $ growSizeM x diff
    {-# INLINE ensureSizeM #-}
 
-- mono kinded imports
instance (V.Vector v a) => M.Convert (Vec v a) (v a) where
    convert (Vec v) = v
    {-# INLINE convert #-}
instance (V.Vector v a) => M.Convert (v a) (Vec v a) where
    convert = Vec
    {-# INLINE convert #-}
instance (V.Vector v a, V.Vector w a) => M.Convert (Vec v a) (Vec w a) where
    convert (Vec v) = Vec (V.convert v)
    {-# INLINE convert #-}

instance (V.Vector v a) => M.Concat (Vec v a) where
    concat l l' = V.concat [l, l']
    {-# INLINE concat #-}
instance (V.Vector v a) => M.MakeNew (Vec v a) where
    makeNew = V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => M.MakeNew (DVec v a) where
    makeNew = DVec V.empty
    {-# INLINE makeNew #-}

-- mono-traversable support
-- type instance Element (Vec v a) = a
-- type instance Element (DVec v a) = a
-- instance (MonoFunctor (v a), Element (v a) ~ a) => MonoFunctor ((Vec v) a) where
--     omap f (Vec l) = Vec (omap f l)
--     {-# inline omap #-}
-- instance (MonoFunctor (v a), Element (v a) ~ a) => MonoFunctor ((DVec v) a) where
--     omap f (DVec l) = DVec (omap f l)
--     {-# inline omap #-}
-- instance (MonoFoldable (v a), Element (v a) ~ a) => MonoFoldable ((Vec v) a) where
--     ofoldMap f (Vec l) = ofoldMap f l
--     {-# INLINE ofoldMap #-}
--     ofoldr f z (Vec l) = ofoldr f z l
--     {-# INLINE ofoldr #-}
--     ofoldl' x b (Vec l) = ofoldl' x b l
--     {-# INLINE ofoldl' #-}
--     ofoldr1Ex x (Vec l) = ofoldr1Ex x l
--     {-# INLINE ofoldr1Ex #-}
--     ofoldl1Ex' x (Vec l) = ofoldl1Ex' x l
--     {-# INLINE ofoldl1Ex' #-}
-- instance (MonoTraversable (v a), Element (v a) ~ a) => MonoTraversable ((Vec v) a) where
--     otraverse f (Vec l) = fmap Vec (otraverse f l)
--     {-# INLINE otraverse #-}
