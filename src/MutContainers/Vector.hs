module MutContainers.Vector (
    Vec, DVec, VecM, DVecM,
    Vector, VectorU, VectorS, VectorM,
    DVector, DVectorU, DVectorS, DVectorM,
)
where
import Prelude
import Control.Monad
import Control.DeepSeq
import MutState.State
import MutContainers.Container
import MutContainers.Map
import MutContainers.Size
import MutContainers.List
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Generic.Mutable   as VM
import qualified Data.Vector                   as VI
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Storable          as VS
-- import qualified Data.Vector.Mutable           as VMI
-- import qualified Data.Vector.Unboxed.Mutable   as VMU
-- import qualified Data.Vector.Storable.Mutable  as VMS

newtype Vec (v :: * -> *) (k :: *) (a :: *) = Vec (v a)
newtype MVec (mv :: * -> * -> *) (s :: *) (k :: *) (a :: *) = MVec (mv s a)
type instance Mut s (Vec v) = MVec (V.Mutable v) s
type instance Cst s (Vec v) = MVec (V.Mutable v) s
type instance Mut s (Vec v k) = MVec (V.Mutable v) s k
type instance Cst s (Vec v k) = MVec (V.Mutable v) s k
type instance Mut s (Vec v k a) = MVec (V.Mutable v) s k a
type instance Cst s (Vec v k a) = MVec (V.Mutable v) s k a
type instance KeyOf (Vec v) = Int
type instance SizeOf (Vec v) = Int
type instance KeyOf (Vec v k) = k
type instance SizeOf (Vec v k) = SizeOf (Vec v)
type instance ValOf (Vec v k a) = a
type instance KeyOf (Vec v k a) = KeyOf (Vec v k)
type instance SizeOf (Vec v k a) = SizeOf (Vec v k)

newtype DVec (v :: * -> *) (k :: *) (a :: *) = DVec (v a)
newtype MDVec (mv :: * -> * -> *) (s :: *) (k :: *) (a :: *) = 
    MDVec (Mut s (Var (mv s a)))
type instance Mut s (DVec v) = MDVec (V.Mutable v) s
type instance Cst s (DVec v) = MDVec (V.Mutable v) s
type instance Mut s (DVec v k) = MDVec (V.Mutable v) s k
type instance Cst s (DVec v k) = MDVec (V.Mutable v) s k
type instance Mut s (DVec v k a) = MDVec (V.Mutable v) s k a
type instance Cst s (DVec v k a) = MDVec (V.Mutable v) s k a
type instance KeyOf (DVec v) = KeyOf (Vec v)
type instance SizeOf (DVec v) = SizeOf (Vec v)
type instance KeyOf (DVec v k) = KeyOf (Vec v k)
type instance SizeOf (DVec v k) = SizeOf (Vec v k)
type instance ValOf (DVec v k a) = ValOf (Vec v k a)
type instance KeyOf (DVec v k a) = KeyOf (Vec v k a)
type instance SizeOf (DVec v k a) = SizeOf (Vec v k a)

newtype VecM (v :: * -> *) (k :: *) (a :: *) = VecM (v a)
newtype MVecM (mv :: * -> * -> *) (s :: *) (k :: *) (a :: *) = MVecM (mv s a)
type instance Mut s (VecM v k a) = MVecM (V.Mutable v) s k (Mut s a)
type instance Cst s (VecM v k a) = MVecM (V.Mutable v) s k (Cst s a)
type instance KeyOf (VecM v) = KeyOf (Vec v)
type instance SizeOf (VecM v) = SizeOf (Vec v)
type instance KeyOf (VecM v k) = KeyOf (Vec v k)
type instance SizeOf (VecM v k) = SizeOf (Vec v k)
type instance ValOf (VecM v k a) = ValOf (Vec v k a)
type instance KeyOf (VecM v k a) = KeyOf (Vec v k a)
type instance SizeOf (VecM v k a) = SizeOf (Vec v k a)

newtype DVecM (v :: * -> *) (k :: *) (a :: *) = DVecM (v a)
newtype MDVecM (mv :: * -> * -> *) (s :: *) (k :: *) (a :: *) = 
    MDVecM (Mut s (Var (mv s a)))
type instance Mut s (DVecM v k a) = MDVecM (V.Mutable v) s k (Mut s a)
type instance Cst s (DVecM v k a) = MDVecM (V.Mutable v) s k (Cst s a)
type instance KeyOf (DVecM v) = KeyOf (Vec v)
type instance SizeOf (DVecM v) = SizeOf (Vec v)
type instance KeyOf (DVecM v k) = KeyOf (Vec v k)
type instance SizeOf (DVecM v k) = SizeOf (Vec v k)
type instance ValOf (DVecM v k a) = ValOf (Vec v k a)
type instance KeyOf (DVecM v k a) = KeyOf (Vec v k a)
type instance SizeOf (DVecM v k a) = SizeOf (Vec v k a)

type Vector = Vec VI.Vector Int
type VectorU = Vec VU.Vector Int
type VectorS = Vec VS.Vector Int
-- type MVector = MVec VMI.MVector Int
-- type MVectorU = MVec VMU.MVector Int
-- type MVectorS = MVec VMS.MVector Int
type DVector = DVec VI.Vector Int
type DVectorU = DVec VU.Vector Int
type DVectorS = DVec VS.Vector Int

type VectorM = VecM VI.Vector Int
type DVectorM = DVecM VI.Vector Int

-- inherit typeclasses -- need to find some automated deriving mechanism
instance (NFData (v a)) => NFData (Vec v k a) where rnf (Vec v) = rnf v
instance NFData (MVec v s k a) where rnf (MVec _) = ()
instance (NFData (v a)) => NFData (DVec v k a) where rnf (DVec v) = rnf v
instance NFData (MDVec v s k a) where rnf (MDVec _) = ()
instance (NFData (v a)) => NFData (VecM v k a) where rnf (VecM v) = rnf v
instance NFData (MVecM v s k a) where rnf (MVecM _) = ()
instance (NFData (v a)) => NFData (DVecM v k a) where rnf (DVecM v) = rnf v
instance NFData (MDVecM v s k a) where rnf (MDVecM _) = ()

instance (Functor v) => Functor (Vec v k) where
    fmap f (Vec l) = Vec (fmap f l)
    {-# inline fmap #-}
instance (Foldable v) => Foldable (Vec v k) where
    foldMap f (Vec l) = foldMap f l
    {-# inline foldMap #-}
    foldr f z (Vec l) = foldr f z l
    {-# inline foldr #-}
instance (Traversable v) => Traversable (Vec v k) where
    traverse f (Vec l) = Vec <$> traverse f l
    {-# inline traverse #-}
    sequenceA (Vec l) = Vec <$> sequenceA l
    {-# inline sequenceA #-}
instance (Applicative v) => Applicative (Vec v k) where
    pure x = Vec (pure x)
    {-# inline pure #-}
    (Vec f) <*> (Vec l) = Vec (f <*> l)
    {-# inline (<*>) #-}
instance (Monad v) => Monad (Vec v k) where
    (Vec ma) >>= f = Vec (ma >>= \x -> let (Vec v) = f x in v)
    {-# inline (>>=) #-}
instance (Semigroup (v a)) => Semigroup (Vec v k a) where
    (Vec v) <> (Vec w) = Vec (v <> w)
    {-# inline (<>) #-}
instance (Monoid (v a)) => Monoid (Vec v k a) where
    mempty = Vec mempty


-- Vector

-- Map

instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => WriteM (Vec v k a) where
    writeM (MVec mv) = VM.unsafeWrite mv
    {-# INLINE writeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => WriteM (DVec v k a) where
    writeM (MDVec vl) k a = readMutV vl >>= \l -> VM.unsafeWrite l k a
    {-# INLINE writeM #-}
instance (forall b . WriteM (Vec v k b)) => WriteMM (VecM v k a) where
    writeMM (MVecM mv) = writeM (MVec mv)
    {-# INLINE writeMM #-}
instance (forall b . WriteM (DVec v k b)) => WriteMM (DVecM v k a) where
    writeMM (MDVecM mv) = writeM (MDVec mv)
    {-# INLINE writeMM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => ReadC (Vec v k a) where
    readC (MVec mv) = VM.unsafeRead mv
    {-# INLINE readC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => ReadC (DVec v k a) where
    readC (MDVec vl) k = readMutV vl >>= \l -> VM.unsafeRead l k
    {-# INLINE readC #-}
instance (forall b . ReadC (Vec v k b)) => ReadCC (VecM v k a) where
    readCC (MVecM mv) = readC (MVec mv)
    {-# INLINE readCC #-}
instance (forall b . ReadC (DVec v k b)) => ReadCC (DVecM v k a) where
    readCC (MDVecM mv) = readC (MDVec mv)
    {-# INLINE readCC #-}
instance (forall b . ReadC (Vec v k b)) => ReadMM (VecM v k a) where
    readMM (MVecM mv) = readC (MVec mv)
    {-# INLINE readMM #-}
instance (forall b . ReadC (DVec v k b)) => ReadMM (DVecM v k a) where
    readMM (MDVecM mv) = readC (MDVec mv)
    {-# INLINE readMM #-}
instance (V.Vector v a, k ~ KeyOf (Vec v)) => ReadAt (Vec v k a) where
    at (Vec v) = (V.!) v
    {-# INLINE at #-}
instance (V.Vector v a, k ~ KeyOf (Vec v)) => ReadAt (DVec v k a) where
    (DVec v) `at` u = v V.! u
    {-# INLINE at #-}
instance (ReadAt (Vec v k a)) => ReadAt (VecM v k a) where
    at (VecM v) = at (Vec v)
    {-# INLINE at #-}
instance (ReadAt (DVec v k a)) => ReadAt (DVecM v k a) where
    at (DVecM v) = at (DVec v)
    {-# INLINE at #-}


--  Size


instance (V.Vector v a) => GetSize (Vec v k a) where
    getSize (Vec v) = V.length v
    {-# INLINE getSize #-}
instance (V.Vector v a) => GetSize (DVec v k a) where
    getSize (DVec v) = V.length v
    {-# INLINE getSize #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => GetSizeC (Vec v k a) where
    getSizeC (MVec mv) = return (VM.length mv)
    {-# INLINE getSizeC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => GetSizeC (DVec v k a) where
    getSizeC (MDVec vl) = readMutV vl >>= \l -> return (VM.length l)
    {-# INLINE getSizeC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => GrowSizeM (DVec v k a) where
    growSizeM (MDVec vl) n = readMutV vl >>= flip VM.unsafeGrow n >>= writeMutV vl
    {-# INLINE growSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ShrinkSizeM (DVec v k a) where
    shrinkSizeM (MDVec vl) n = readMutV vl >>= \l -> writeMutV vl (VM.unsafeSlice 0 n l)
    {-# INLINE shrinkSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ModifySizeM (DVec v k a) where
    modifySizeM x f = do
        let MDVec vl = x
        size <- readMutV vl >>= \l -> return (VM.length l)
        let diff = f size - size
        if diff > 0 then growSizeM x diff
        else when (diff < 0) $ shrinkSizeM x (-diff)
    {-# INLINE modifySizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => SetSizeM (DVec v k a)
instance (mv ~ V.Mutable v, VM.MVector mv a) => EnsureSizeM (DVec v k a) where
    ensureSizeM x z = do
        let MDVec vl = x
        size <- readMutV vl >>= \l -> return (VM.length l)
        let diff = z - size
        when (diff > 0) $ growSizeM x diff
    {-# INLINE ensureSizeM #-}

instance (V.Vector v a) => GetSize (VecM v k a) where
    getSize (VecM v) = getSize (Vec v)
instance (V.Vector v a) => GetSize (DVecM v k a) where
    getSize (DVecM v) = getSize (DVec v)
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => GetSizeC (VecM v k a) where
    getSizeC (MVecM mv) = getSizeC (MVec mv)
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => GetSizeC (DVecM v k a) where
    getSizeC (MDVecM mv) = getSizeC (MDVec mv)
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => GrowSizeM (DVecM v k a) where
    growSizeM (MDVecM mv) = growSizeM (MDVec mv)
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => ShrinkSizeM (DVecM v k a) where
    shrinkSizeM (MDVecM mv) = shrinkSizeM (MDVec mv)
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => ModifySizeM (DVecM v k a) where
    modifySizeM (MDVecM mv) = modifySizeM (MDVec mv)
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => SetSizeM (DVecM v k a) where
    setSizeM (MDVecM mv) = setSizeM (MDVec mv)
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => EnsureSizeM (DVecM v k a) where
    ensureSizeM (MDVecM mv) = ensureSizeM (MDVec mv)


-- Container

instance (V.Vector v a) => MakeNew (Vec v k a) where
    makeNew = Vec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => MakeNew (DVec v k a) where
    makeNew = DVec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => Convert (Vec v k a) (v a) where
    convert (Vec v) = v
    {-# INLINE convert #-}
instance (V.Vector v a) => Convert (v a) (Vec v k a) where
    convert = Vec
    {-# INLINE convert #-}
instance (V.Vector v a, V.Vector w a) => Convert (Vec v k a) (Vec w k a) where
    convert (Vec v) = Vec (V.convert v)
    {-# INLINE convert #-}

instance (mv ~ V.Mutable v, VM.MVector mv a) => MakeNewM (Vec v k a) where
    makeNewM = MVec <$> VM.new 0
    {-# INLINE makeNewM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => MakeNewM (DVec v k a) where
    makeNewM = MDVec <$> (VM.new 0 >>= newMutV)
    {-# INLINE makeNewM #-}
instance (forall b . V.Vector v b, UThawM (VecM v k a)) => MakeNewM (VecM v k a) where
    makeNewM = uthawM (VecM V.empty)
    {-# INLINE makeNewM #-}
instance (forall b . V.Vector v b, UThawM (DVecM v k a)) => MakeNewM (DVecM v k a) where
    makeNewM = uthawM (DVecM V.empty)
    {-# INLINE makeNewM #-}
instance (V.Vector v a) => FreezeC (Vec v k a) where
    freezeC (MVec mv) = Vec <$> V.freeze mv
    {-# INLINE freezeC #-}
instance (V.Vector v a) => FreezeC (DVec v k a) where
    freezeC (MDVec vl) = DVec <$> (readMutV vl >>= V.freeze)
    {-# INLINE freezeC #-}
instance (V.Vector v a) => UFreezeC (Vec v k a) where
    ufreezeC (MVec mv) = Vec <$> V.unsafeFreeze mv
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => UFreezeC (DVec v k a) where
    ufreezeC (MDVec vl) = DVec <$> (readMutV vl >>= V.unsafeFreeze)
    {-# INLINE ufreezeC #-}
instance (forall b . V.Vector v b, UFreezeC a) => UFreezeC (VecM v k a) where
    ufreezeC (MVecM mv) = VecM <$> (V.unsafeFreeze mv >>= V.mapM ufreezeC) -- O(n)
    {-# INLINE ufreezeC #-}
instance (forall b . V.Vector v b, UFreezeC a) => UFreezeC (DVecM v k a) where
    ufreezeC (MDVecM mv) = DVecM <$> (readMutV mv >>= V.unsafeFreeze >>= V.mapM ufreezeC) -- O(n)
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => ThawM (Vec v k a) where
    thawM (Vec v) = MVec <$> V.thaw v
    {-# INLINE thawM #-}
instance (V.Vector v a) => ThawM (DVec v k a) where
    thawM (DVec l) = MDVec <$> (V.thaw l >>= newMutV)
    {-# INLINE thawM #-}
instance (V.Vector v a) => UThawM (Vec v k a) where
    uthawM (Vec v) = MVec <$> V.unsafeThaw v
    {-# INLINE uthawM #-}
instance (V.Vector v a) => UThawM (DVec v k a) where
    uthawM (DVec l) = MDVec <$> (V.unsafeThaw l >>= newMutV)
    {-# INLINE uthawM #-}
instance (forall b . V.Vector v b, UThawM a) => UThawM (VecM v k a) where
    uthawM (VecM mv) = MVecM <$> (V.mapM uthawM mv >>= V.unsafeThaw) -- O(n)
    {-# INLINE uthawM #-}
instance (forall b . V.Vector v b, UThawM a) => UThawM (DVecM v k a) where
    uthawM (DVecM mv) = MDVecM <$> (V.mapM uthawM mv >>= V.unsafeThaw >>= newMutV) -- O(n)
    {-# INLINE uthawM #-}



-- List

instance (forall b . V.Vector v b) => Zip (Vec v k) where
    zip (Vec v) (Vec v') = Vec (V.zip v v')
    {-# INLINE zip #-}
instance (forall b . V.Vector v b) => Map (Vec v k) where
    map f (Vec v) = Vec (V.map f v)
    {-# INLINE map #-}
instance (forall b . V.Vector v b) => EnumFromTo (Vec v k) where
    enumFromTo a b = Vec $ V.enumFromTo a b --Vec $ V.enumFromN a (b - a + 1)
    {-# INLINE enumFromTo #-}
instance (forall b . V.Vector v b) => Concat (Vec v k) where
    concat (Vec v) (Vec v') = Vec $ V.concat [v, v']
    {-# INLINE concat #-}
instance (forall b . V.Vector v b) => Replicate (Vec v k) where
    replicate n x = Vec $ V.replicate n x
    {-# INLINE replicate #-}
instance (forall b . V.Vector v b) => ToList (Vec v k) where
    toList (Vec v) = V.toList v
    {-# INLINE toList #-}
instance (forall b . V.Vector v b) => FromList (Vec v k) where
    fromList = Vec . V.fromList
    {-# INLINE fromList #-}


instance (mv ~ V.Mutable v, VM.MVector mv a) => ReplicateM (Vec v k a) where
    replicateM n x = MVec <$> VM.replicateM n x
    {-# INLINE replicateM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ReplicateM (DVec v k a) where
    replicateM n v = MDVec <$> (VM.replicateM n v >>= newMutV)
    {-# INLINE replicateM #-}



-- mono-traversable support
-- type instance Element (Vec v k a) = a
-- type instance Element (DVec v a) = a
-- instance (MonoFunctor (v a), Element (v a) ~ a) => MonoFunctor ((Vec v k) a) where
--     omap f (Vec l) = Vec (omap f l)
--     {-# inline omap #-}
-- instance (MonoFunctor (v a), Element (v a) ~ a) => MonoFunctor ((DVec v) a) where
--     omap f (DVec l) = DVec (omap f l)
--     {-# inline omap #-}
-- instance (MonoFoldable (v a), Element (v a) ~ a) => MonoFoldable ((Vec v k) a) where
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
-- instance (MonoTraversable (v a), Element (v a) ~ a) => MonoTraversable ((Vec v k) a) where
--     otraverse f (Vec l) = fmap Vec (otraverse f l)
--     {-# INLINE otraverse #-}
