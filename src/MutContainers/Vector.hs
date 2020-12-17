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
import qualified MutContainers.Tri.Map as T
import qualified MutContainers.Bi.Container as B
import qualified MutContainers.Bi.Map as B
import qualified MutContainers.Bi.Size as B
import qualified MutContainers.Bi.List as B
import qualified MutContainers.Mono.Container as M
import qualified MutContainers.Mono.Map as M
import qualified MutContainers.Mono.List as M
import qualified MutContainers.Mono.Size as M
import qualified Data.Vector.Generic           as V
import qualified Data.Vector.Generic.Mutable   as VM
import qualified Data.Vector                   as VI
import qualified Data.Vector.Unboxed           as VU
import qualified Data.Vector.Storable          as VS
-- import qualified Data.Vector.Mutable           as VMI
-- import qualified Data.Vector.Unboxed.Mutable   as VMU
-- import qualified Data.Vector.Storable.Mutable  as VMS
import MutContainers.Any.Map
import MutContainers.Any.Size

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
-- tri kinded imports

instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => T.WriteM (Vec v) k a where
    writeM (MVec mv) = VM.unsafeWrite mv
    {-# INLINE writeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => T.WriteM (DVec v) k a where
    writeM (MDVec vl) k a = readMutV vl >>= \l -> VM.unsafeWrite l k a
    {-# INLINE writeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => T.ReadC (Vec v) k a where
    readC (MVec mv) = VM.unsafeRead mv
    {-# INLINE readC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a, k ~ KeyOf (Vec v)) => T.ReadC (DVec v) k a where
    readC (MDVec vl) k = readMutV vl >>= \l -> VM.unsafeRead l k
    {-# INLINE readC #-}
instance (V.Vector v a, k ~ KeyOf (Vec v)) => T.ReadAt (Vec v) k a where
    at (Vec v) = (V.!) v
    {-# INLINE at #-}
instance (V.Vector v a, k ~ KeyOf (Vec v)) => T.ReadAt (DVec v) k a where
    (DVec v) `at` u = v V.! u
    {-# INLINE at #-}

-- bi kinded imports

instance (T.WriteM (Vec v) k a) => B.WriteM (Vec v k) a where
    writeM = T.writeM
    {-# INLINE writeM #-}
instance (T.WriteM (DVec v) k a) => B.WriteM (DVec v k) a where
    writeM = T.writeM
    {-# INLINE writeM #-}
instance (T.ReadC (Vec v) k a) => B.ReadC (Vec v k) a where
    readC = T.readC
    {-# INLINE readC #-}
instance (T.ReadC (DVec v) k a) => B.ReadC (DVec v k) a where
    readC = T.readC
    {-# INLINE readC #-}
instance (T.ReadAt (Vec v) k a) => B.ReadAt (Vec v k) a where
    at = T.at
    {-# INLINE at #-}
instance (T.ReadAt (DVec v) k a) => B.ReadAt (DVec v k) a where
    at = T.at
    {-# INLINE at #-}

instance (V.Vector v a) => B.FreezeC (Vec v k) a where
    freezeC (MVec mv) = Vec <$> V.freeze mv
    {-# INLINE freezeC #-}
instance (V.Vector v a) => B.FreezeC (DVec v k) a where
    freezeC (MDVec vl) = DVec <$> (readMutV vl >>= V.freeze)
    {-# INLINE freezeC #-}
instance (V.Vector v a) => B.UFreezeC (Vec v k) a where
    ufreezeC (MVec mv) = Vec <$> V.unsafeFreeze mv
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => B.UFreezeC (DVec v k) a where
    ufreezeC (MDVec vl) = DVec <$> (readMutV vl >>= V.unsafeFreeze)
    {-# INLINE ufreezeC #-}
instance (V.Vector v a) => B.ThawM (Vec v k) a where
    thawM (Vec v) = MVec <$> V.thaw v
    {-# INLINE thawM #-}
instance (V.Vector v a) => B.ThawM (DVec v k) a where
    thawM (DVec l) = MDVec <$> (V.thaw l >>= newMutV)
    {-# INLINE thawM #-}
instance (V.Vector v a) => B.UThawM (Vec v k) a where
    uthawM (Vec v) = MVec <$> V.unsafeThaw v
    {-# INLINE uthawM #-}
instance (V.Vector v a) => B.UThawM (DVec v k) a where
    uthawM (DVec l) = MDVec <$> (V.unsafeThaw l >>= newMutV)
    {-# INLINE uthawM #-}

instance (V.Vector v a) => B.Convert (Vec v k) v a where
    convert (Vec v) = v
    {-# INLINE convert #-}
instance (V.Vector v a) => B.Convert v (Vec v k) a where
    convert = Vec
    {-# INLINE convert #-}
instance (V.Vector v a, V.Vector w a) => B.Convert (Vec v k) (Vec w k) a where
    convert (Vec v) = Vec (V.convert v)
    {-# INLINE convert #-}

instance (V.Vector v a, V.Vector v b, V.Vector v (a, b)) => B.Zip (Vec v k) a b where
    zip (Vec v) (Vec v') = Vec (V.zip v v')
    {-# INLINE zip #-}
instance (V.Vector v a, V.Vector v b) => B.Map (Vec v k) a b where
    map f (Vec v) = Vec (V.map f v)
    {-# INLINE map #-}
instance (V.Vector v Int) => B.EnumFromTo (Vec v k) Int where
    enumFromTo a b = Vec $ V.enumFromN a (b - a + 1)
    {-# INLINE enumFromTo #-}
instance (V.Vector v a) => B.Concat (Vec v k) a where
    concat (Vec v) (Vec v') = Vec $ V.concat [v, v']
    {-# INLINE concat #-}
instance (V.Vector v a) => B.Replicate (Vec v k) a where
    replicate n x = Vec $ V.replicate n x
    {-# INLINE replicate #-}
instance (V.Vector v a) => B.ToList (Vec v k) a where
    toList (Vec v) = V.toList v
    {-# INLINE toList #-}
instance (V.Vector v a) => B.FromList (Vec v k) a where
    fromList = Vec . V.fromList
    {-# INLINE fromList #-}
instance (V.Vector v a) => B.MakeNew (Vec v k) a where
    makeNew = Vec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => B.MakeNew (DVec v k) a where
    makeNew = DVec V.empty
    {-# INLINE makeNew #-}
instance (V.Vector v a) => B.GetSize (Vec v k) a where
    getSize (Vec v) = V.length v
    {-# INLINE getSize #-}
instance (V.Vector v a) => B.GetSize (DVec v k) a where
    getSize (DVec v) = V.length v
    {-# INLINE getSize #-}

instance (mv ~ V.Mutable v, VM.MVector mv a) => B.ReplicateM (Vec v k) a where
    replicateM n x = MVec <$> VM.replicateM n x
    {-# INLINE replicateM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.ReplicateM (DVec v k) a where
    replicateM n v = MDVec <$> (VM.replicateM n v >>= newMutV)
    {-# INLINE replicateM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.MakeNewM (Vec v k) a where
    makeNewM = MVec <$> VM.new 0
    {-# INLINE makeNewM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.MakeNewM (DVec v k) a where
    makeNewM = MDVec <$> (VM.new 0 >>= newMutV)
    {-# INLINE makeNewM #-}

instance (mv ~ V.Mutable v, VM.MVector mv a) => B.GetSizeC (Vec v k) a where
    getSizeC (MVec mv) = return (VM.length mv)
    {-# INLINE getSizeC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.GetSizeC (DVec v k) a where
    getSizeC (MDVec vl) = readMutV vl >>= \l -> return (VM.length l)
    {-# INLINE getSizeC #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.GrowSizeM (DVec v k) a where
    growSizeM (MDVec vl) n = readMutV vl >>= flip VM.unsafeGrow n >>= writeMutV vl
    {-# INLINE growSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.ShrinkSizeM (DVec v k) a where
    shrinkSizeM (MDVec vl) n = readMutV vl >>= \l -> writeMutV vl (VM.unsafeSlice 0 n l)
    {-# INLINE shrinkSizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.ModifySizeM (DVec v k) a where
    modifySizeM x f = do
        let MDVec vl = x
        size <- readMutV vl >>= \l -> return (VM.length l)
        let diff = f size - size
        if diff > 0 then B.growSizeM x diff
        else when (diff < 0) $ B.shrinkSizeM x (-diff)
    {-# INLINE modifySizeM #-}
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.SetSizeM (DVec v k) a
instance (mv ~ V.Mutable v, VM.MVector mv a) => B.EnsureSizeM (DVec v k) a where
    ensureSizeM x z = do
        let MDVec vl = x
        size <- readMutV vl >>= \l -> return (VM.length l)
        let diff = z - size
        when (diff > 0) $ B.growSizeM x diff
    {-# INLINE ensureSizeM #-}
 
-- mono kinded imports
instance (T.WriteM (Vec v) k a) => M.WriteM (Vec v k a) where
    writeM = T.writeM
    {-# INLINE writeM #-}
instance (T.WriteM (DVec v) k a) => M.WriteM (DVec v k a) where
    writeM = T.writeM
    {-# INLINE writeM #-}
instance (forall b . M.WriteM (Vec v k b)) => M.WriteMM (VecM v k a) where
    writeMM (MVecM mv) = M.writeM (MVec mv)
    {-# INLINE writeMM #-}
instance (forall b . M.WriteM (DVec v k b)) => M.WriteMM (DVecM v k a) where
    writeMM (MDVecM mv) = M.writeM (MDVec mv)
    {-# INLINE writeMM #-}
instance (T.ReadC (Vec v) k a) => M.ReadC (Vec v k a) where
    readC = T.readC
    {-# INLINE readC #-}
instance (T.ReadC (DVec v) k a) => M.ReadC (DVec v k a) where
    readC = T.readC
    {-# INLINE readC #-}
instance (forall b . M.ReadC (Vec v k b)) => M.ReadCC (VecM v k a) where
    readCC (MVecM mv) = M.readC (MVec mv)
    {-# INLINE readCC #-}
instance (forall b . M.ReadC (DVec v k b)) => M.ReadCC (DVecM v k a) where
    readCC (MDVecM mv) = M.readC (MDVec mv)
    {-# INLINE readCC #-}
instance (forall b . M.ReadC (Vec v k b)) => M.ReadMM (VecM v k a) where
    readMM (MVecM mv) = M.readC (MVec mv)
    {-# INLINE readMM #-}
instance (forall b . M.ReadC (DVec v k b)) => M.ReadMM (DVecM v k a) where
    readMM (MDVecM mv) = M.readC (MDVec mv)
    {-# INLINE readMM #-}
instance (T.ReadAt (Vec v) k a) => M.ReadAt (Vec v k a) where
    at = T.at
    {-# INLINE at #-}
instance (T.ReadAt (DVec v) k a) => M.ReadAt (DVec v k a) where
    at = T.at
    {-# INLINE at #-}
instance (M.ReadAt (Vec v k a)) => M.ReadAt (VecM v k a) where
    at (VecM v) = M.at (Vec v)
    {-# INLINE at #-}
instance (M.ReadAt (DVec v k a)) => M.ReadAt (DVecM v k a) where
    at (DVecM v) = M.at (DVec v)
    {-# INLINE at #-}

instance (B.GetSizeC (Vec v k) a) => M.GetSizeC (Vec v k a) where
    getSizeC = B.getSizeC
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => M.GetSizeC (VecM v k a) where
    getSizeC (MVecM mv) = M.getSizeC (MVec mv)
instance (B.GetSizeC (DVec v k) a) => M.GetSizeC (DVec v k a) where
    getSizeC = B.getSizeC
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => M.GetSizeC (DVecM v k a) where
    getSizeC (MDVecM mv) = M.getSizeC (MDVec mv)
instance (B.GrowSizeM (DVec v k) a) => M.GrowSizeM (DVec v k a) where
    growSizeM = B.growSizeM
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => M.GrowSizeM (DVecM v k a) where
    growSizeM (MDVecM mv) = M.growSizeM (MDVec mv)
instance (B.ShrinkSizeM (DVec v k) a) => M.ShrinkSizeM (DVec v k a) where
    shrinkSizeM = B.shrinkSizeM
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => M.ShrinkSizeM (DVecM v k a) where
    shrinkSizeM (MDVecM mv) = M.shrinkSizeM (MDVec mv)
instance (B.ModifySizeM (DVec v k) a) => M.ModifySizeM (DVec v k a) where
    modifySizeM = B.modifySizeM
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => M.ModifySizeM (DVecM v k a) where
    modifySizeM (MDVecM mv) = M.modifySizeM (MDVec mv)
instance (B.SetSizeM (DVec v k) a) => M.SetSizeM (DVec v k a) where
    setSizeM = B.setSizeM
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => M.SetSizeM (DVecM v k a) where
    setSizeM (MDVecM mv) = M.setSizeM (MDVec mv)
instance (B.EnsureSizeM (DVec v k) a) => M.EnsureSizeM (DVec v k a) where
    ensureSizeM = B.ensureSizeM
instance (mv ~ V.Mutable v, forall b . VM.MVector mv b) => M.EnsureSizeM (DVecM v k a) where
    ensureSizeM (MDVecM mv) = M.ensureSizeM (MDVec mv)

instance (B.FreezeC (Vec v k) a) => M.FreezeC (Vec v k a) where
    freezeC = B.freezeC
instance (B.FreezeC (DVec v k) a) => M.FreezeC (DVec v k a) where
    freezeC = B.freezeC
instance (B.UFreezeC (Vec v k) a) => M.UFreezeC (Vec v k a) where
    ufreezeC = B.ufreezeC
instance (B.UFreezeC (DVec v k) a) => M.UFreezeC (DVec v k a) where
    ufreezeC = B.ufreezeC
instance (forall b . V.Vector v b, M.UFreezeC a) => M.UFreezeC (VecM v k a) where
    ufreezeC (MVecM mv) = VecM <$> (V.unsafeFreeze mv >>= V.mapM M.ufreezeC) -- O(n)
instance (forall b . V.Vector v b, M.UFreezeC a) => M.UFreezeC (DVecM v k a) where
    ufreezeC (MDVecM mv) = DVecM <$> (readMutV mv >>= V.unsafeFreeze >>= V.mapM M.ufreezeC) -- O(n)
instance (B.ThawM (Vec v k) a) => M.ThawM (Vec v k a) where
    thawM = B.thawM
instance (B.ThawM (DVec v k) a) => M.ThawM (DVec v k a) where
    thawM = B.thawM
instance (B.UThawM (Vec v k) a) => M.UThawM (Vec v k a) where
    uthawM = B.uthawM
instance (B.UThawM (DVec v k) a) => M.UThawM (DVec v k a) where
    uthawM = B.uthawM
instance (forall b . V.Vector v b, M.UThawM a) => M.UThawM (VecM v k a) where
    uthawM (VecM mv) = MVecM <$> (V.mapM M.uthawM mv >>= V.unsafeThaw) -- O(n)
instance (forall b . V.Vector v b, M.UThawM a) => M.UThawM (DVecM v k a) where
    uthawM (DVecM mv) = MDVecM <$> (V.mapM M.uthawM mv >>= V.unsafeThaw >>= newMutV) -- O(n)
instance (B.MakeNewM (Vec v k) a) => M.MakeNewM (Vec v k a) where
    makeNewM = B.makeNewM
instance (B.MakeNewM (DVec v k) a) => M.MakeNewM (DVec v k a) where
    makeNewM = B.makeNewM
instance (forall b . V.Vector v b, M.UThawM (VecM v k a)) => M.MakeNewM (VecM v k a) where
    makeNewM = M.uthawM (VecM V.empty)
instance (forall b . V.Vector v b, M.UThawM (DVecM v k a)) => M.MakeNewM (DVecM v k a) where
    makeNewM = M.uthawM (DVecM V.empty)

instance (B.Convert (Vec v k) v a) => M.Convert (Vec v k a) (v a) where
    convert = B.convert
instance (B.Convert v (Vec v k) a) => M.Convert (v a) (Vec v k a) where
    convert = B.convert
instance (B.Convert (Vec v k) (Vec w k) a) => M.Convert (Vec v k a) (Vec w k a) where
    convert = B.convert

instance (B.EnumFromTo (Vec v k) Int) => M.EnumFromTo (Vec v k Int) where
    enumFromTo = B.enumFromTo
instance (B.Concat (Vec v k) a) => M.Concat (Vec v k a) where
    concat = B.concat
instance (B.Replicate (Vec v k) a) => M.Replicate (Vec v k a) where
    replicate = B.replicate
instance (B.MakeNew (Vec v k) a) => M.MakeNew (Vec v k a) where
    makeNew = B.makeNew
instance (B.MakeNew (DVec v k) a) => M.MakeNew (DVec v k a) where
    makeNew = B.makeNew
instance (B.GetSize (Vec v k) a) => M.GetSize (Vec v k a) where
    getSize = B.getSize
instance (B.GetSize (DVec v k) a) => M.GetSize (DVec v k a) where
    getSize = B.getSize
instance (V.Vector v a) => M.GetSize (VecM v k a) where
    getSize (VecM v) = M.getSize (Vec v)
instance (V.Vector v a) => M.GetSize (DVecM v k a) where
    getSize (DVecM v) = M.getSize (DVec v)


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
