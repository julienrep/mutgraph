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

newtype Vec (v :: * -> *) (k :: *) (a :: *) = Vec (v a)
newtype MVec (mv :: * -> * -> *) (s :: *) (k :: *) (a :: *) = MVec (mv s a)
type instance Mut s (Vec v k) = MVec (V.Mutable v) s k
type instance Cst s (Vec v k) = MVec (V.Mutable v) s k

newtype DVec (v :: * -> *) (k :: *) (a :: *) = DVec (v a)
newtype MDVec (mv :: * -> * -> *) (s :: *) (k :: *) (a :: *) = 
    MDVec (Mut s (MutV (mv s a)))
type instance Mut s (DVec v k) = MDVec (V.Mutable v) s k
type instance Cst s (DVec v k) = MDVec (V.Mutable v) s k

type Vector = Vec VI.Vector Int
type VectorU = Vec VU.Vector Int
type VectorS = Vec VS.Vector Int
type MVector = MVec VMI.MVector Int
type MVectorU = MVec VMU.MVector Int
type MVectorS = MVec VMS.MVector Int
type DVector = DVec VI.Vector Int
type DVectorU = DVec VU.Vector Int
type DVectorS = DVec VS.Vector Int

-- inherit typeclasses -- need to find some automated deriving mechanism
instance (NFData (v a)) => NFData (Vec v k a) where
  rnf (Vec v) = rnf v
  {-# INLINE rnf #-}
instance NFData (MVec v s k a) where
  rnf (MVec _) = ()
  {-# INLINE rnf #-}
instance (NFData (v a)) => NFData (DVec v k a) where
  rnf (DVec v) = rnf v
  {-# INLINE rnf #-}
instance NFData (MDVec v s k a) where
    rnf (MDVec _) = ()
    {-# INLINE rnf #-}

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

type instance KeyOf (Vec v k) = Int
type instance KeyOf (DVec v k) = Int
type instance SizeOf (Vec v k) = KeyOf (Vec v k)
type instance SizeOf (DVec v k) = KeyOf (DVec v k)
type instance M.SizeOf (Vec v k a) = KeyOf (Vec v k)
type instance M.SizeOf (DVec v k a) = KeyOf (DVec v k)

instance (mv ~ V.Mutable v, VM.MVector mv a) => WriteM (Vec v k) a where
    writeM (MVec mv) = VM.unsafeWrite mv
    {-# INLINE writeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => WriteM (DVec v k) a where
    writeM (MDVec vl) k a = readMutV vl >>= \l -> VM.unsafeWrite l k a
    {-# INLINE writeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ReadC (Vec v k) a where
    readC (MVec mv) = VM.unsafeRead mv
    {-# INLINE readC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ReadC (DVec v k) a where
    readC (MDVec vl) k = readMutV vl >>= \l -> VM.unsafeRead l k
    {-# INLINE readC #-}
instance (V.Vector v a) => ReadAt (Vec v k) a where
    at (Vec v) = (V.!) v
    {-# INLINE at #-}
instance (V.Vector v a) => ReadAt (DVec v k) a where
    (DVec v) `at` u = v V.! u
    {-# INLINE at #-}

instance (V.Vector v a) => FreezeC (Vec v k) a where
    freezeC (MVec mv) = Vec <$> V.freeze mv
    {-# INLINE freezeC #-}
instance (V.Vector v a) => FreezeC (DVec v k) a where
    freezeC (MDVec vl) = DVec <$> (readMutV vl >>= V.freeze)
    {-# INLINE freezeC #-}
instance (V.Vector v a) => UFreezeC (Vec v k) a where
    ufreezeC (MVec mv) = Vec <$> V.unsafeFreeze mv
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => UFreezeC (DVec v k) a where
    ufreezeC (MDVec vl) = DVec <$> (readMutV vl >>= V.unsafeFreeze)
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => ThawM (Vec v k) a where
    thawM (Vec v) = MVec <$> V.thaw v
    {-# INLINE thawM #-}
instance (V.Vector v a) => ThawM (DVec v k) a where
    thawM (DVec l) = MDVec <$> (V.thaw l >>= newMutV)
    {-# INLINE thawM #-}
instance (V.Vector v a) => UThawM (Vec v k) a where
    uthawM (Vec v) = MVec <$> V.unsafeThaw v
    {-# INLINE uthawM #-}
instance (V.Vector v a) => UThawM (DVec v k) a where
    uthawM (DVec l) = MDVec <$> (V.unsafeThaw l >>= newMutV)
    {-# INLINE uthawM #-}

instance (V.Vector v a) => Convert (Vec v k) v a where
    convert (Vec v) = v
    {-# INLINE convert #-}
instance (V.Vector v a) => Convert v (Vec v k) a where
    convert = Vec
    {-# INLINE convert #-}
instance (V.Vector v a, V.Vector w a) => Convert (Vec v k) (Vec w k) a where
    convert (Vec v) = Vec (V.convert v)
    {-# INLINE convert #-}

instance (V.Vector v a, V.Vector v b, V.Vector v (a, b)) => Zip (Vec v k) a b where
    zip (Vec v) (Vec v') = Vec (V.zip v v')
    {-# INLINE zip #-}
instance (V.Vector v a, V.Vector v b) => Map (Vec v k) a b where
    map f (Vec v) = Vec (V.map f v)
    {-# INLINE map #-}
instance (V.Vector v Int) => EnumFromTo (Vec v k) Int where
    enumFromTo a b = Vec $ V.enumFromN a (b - a + 1)
    {-# INLINE enumFromTo #-}
instance (V.Vector v a) => Concat (Vec v k) a where
    concat (Vec v) (Vec v') = Vec $ V.concat [v, v']
    {-# INLINE concat #-}
instance (V.Vector v a) => Replicate (Vec v k) a where
    replicate n x = Vec $ V.replicate n x
    {-# INLINE replicate #-}
instance (V.Vector v a) => ToList (Vec v k) a where
    toList (Vec v) = V.toList v
    {-# INLINE toList #-}
instance (V.Vector v a) => FromList (Vec v k) a where
    fromList = Vec . V.fromList
    {-# INLINE fromList #-}
instance (V.Vector v a) => MakeNew (Vec v k) a where
    makeNew = Vec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => MakeNew (DVec v k) a where
    makeNew = DVec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => GetSize (Vec v k) a where
    getSize (Vec v) = V.length v
    {-# INLINE getSize #-}
instance (V.Vector v a) => GetSize (DVec v k) a where
    getSize (DVec v) = V.length v
    {-# INLINE getSize #-}

instance (mv ~ V.Mutable v, VM.MVector mv a) => ReplicateM (Vec v k) a where
    replicateM n x = MVec <$> VM.replicateM n x
    {-# INLINE replicateM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ReplicateM (DVec v k) a where
    replicateM n v = MDVec <$> (VM.replicateM n v >>= newMutV)
    {-# INLINE replicateM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => MakeNewM (Vec v k) a where
    makeNewM = MVec <$> VM.new 0
    {-# INLINE makeNewM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => MakeNewM (DVec v k) a where
    makeNewM = MDVec <$> (VM.new 0 >>= newMutV)
    {-# INLINE makeNewM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => GetSizeC (Vec v k) a where
    getSizeC (MVec mv) = return (VM.length mv)
    {-# INLINE getSizeC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => GetSizeC (DVec v k) a where
    getSizeC (MDVec vl) = readMutV vl >>= \l -> return (VM.length l)
    {-# INLINE getSizeC #-}

instance (mv ~ V.Mutable v, VM.MVector mv a) => GrowSizeM (DVec v k) a where
    growSizeM (MDVec vl) n = readMutV vl >>= flip VM.unsafeGrow n >>= writeMutV vl
    {-# INLINE growSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ShrinkSizeM (DVec v k) a where
    shrinkSizeM (MDVec vl) n = readMutV vl >>= \l -> writeMutV vl (VM.unsafeSlice 0 n l)
    {-# INLINE shrinkSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => ModifySizeM (DVec v k) a where
    modifySizeM x f = do
        let MDVec vl = x
        size <- readMutV vl >>= \l -> return (VM.length l)
        let diff = (f size) - size
        if diff > 0 then growSizeM x diff
        else when (diff < 0) $ shrinkSizeM x diff
    {-# INLINE modifySizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => SetSizeM (DVec v k) a
instance (mv ~ V.Mutable v, VM.MVector mv a) => EnsureSizeM (DVec v k) a where
    ensureSizeM x z = do
        let MDVec vl = x
        size <- readMutV vl >>= \l -> return (VM.length l)
        let diff = z - size
        when (diff > 0) $ growSizeM x diff
    {-# INLINE ensureSizeM #-}
 
-- mono kinded imports
instance (V.Vector v a) => M.Convert (Vec v k a) (v a) where
    convert (Vec v) = v
    {-# INLINE convert #-}
instance (V.Vector v a) => M.Convert (v a) (Vec v k a) where
    convert = Vec
    {-# INLINE convert #-}
instance (V.Vector v a, V.Vector w a) => M.Convert (Vec v k a) (Vec w k a) where
    convert (Vec v) = Vec (V.convert v)
    {-# INLINE convert #-}

instance (V.Vector v a) => M.Concat (Vec v k a) where
    concat (Vec v) (Vec v') = Vec $ V.concat [v, v']
    {-# INLINE concat #-}
instance (V.Vector v a) => M.MakeNew (Vec v k a) where
    makeNew = Vec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => M.MakeNew (DVec v k a) where
    makeNew = DVec V.empty
    {-# INLINE makeNew #-}

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
