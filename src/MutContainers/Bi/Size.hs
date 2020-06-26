module MutContainers.Bi.Size (
        SizeOf,

        GetSize(..),

        GetSizeC(..), GetSizeCC(..),
        GrowSizeM(..), GrowSizeMM(..),
        ShrinkSizeM(..), ShrinkSizeMM(..),
        ModifySizeM(..), ModifySizeMM(..),
        SetSizeM(..), SetSizeMM(..),
        EnsureSizeM(..), EnsureSizeMM(..),
    )
where
import Prelude hiding (length, zip, map, enumFromTo)
import Control.Monad (when)
import MutState.State

type family SizeOf (l :: * -> *) :: *

class GetSize (l :: * -> *) a where getSize :: l a -> SizeOf l

class GetSizeC (l :: * -> *) a where getSizeC :: (MutMonad s m) => (Cst s l) a -> m (SizeOf l)
class GetSizeCC (l :: * -> *) a where getSizeCC :: (MutMonad s m) => (Cst s l) (Cst s a) -> m (SizeOf l)
instance (forall b. GetSizeC l b) => GetSizeCC (l :: * -> *) a where getSizeCC = getSizeC
class GrowSizeM x a where
    growSizeM :: (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    default growSizeM :: (ModifySizeM x a, Num z) => (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    growSizeM x z = modifySizeM x (+z)
    {-# INLINE growSizeM #-}
class GrowSizeMM x a where growSizeMM :: (MutMonad s m, z ~ SizeOf x) => Mut s x (Mut s a) -> z -> m ()
instance (forall b. GrowSizeM l b) => GrowSizeMM (l :: * -> *) a where growSizeMM = growSizeM
class ShrinkSizeM x a where
    shrinkSizeM :: (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    default shrinkSizeM :: (ModifySizeM x a, Num z) =>
        (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    shrinkSizeM x z = modifySizeM x (+(-z))
    {-# INLINE shrinkSizeM #-}
class ShrinkSizeMM x a where shrinkSizeMM :: (MutMonad s m, z ~ SizeOf x) => Mut s x (Mut s a) -> z -> m ()
instance (forall b. ShrinkSizeM l b) => ShrinkSizeMM (l :: * -> *) a where shrinkSizeMM = shrinkSizeM
class ModifySizeM x a where
    modifySizeM :: (MutMonad s m, z ~ SizeOf x) => Mut s x a -> (z -> z) -> m ()
    default modifySizeM :: (GetSizeC x a, GrowSizeM x a, ShrinkSizeM x a, Num z, Ord z, MutToCstC x a) =>
        (MutMonad s m, z ~ SizeOf x) => Mut s x a -> (z -> z) -> m ()
    modifySizeM x f = do
        size <- getSizeC (cstC x) 
        let diff = (f size) - size
        if diff > 0 then growSizeM x diff
        else when (diff < 0) $ shrinkSizeM x diff
    {-# INLINE modifySizeM #-}
class ModifySizeMM x a where modifySizeMM :: (MutMonad s m, z ~ SizeOf x) => Mut s x (Mut s a) -> (z -> z) -> m ()
instance (forall b. ModifySizeM l b) => ModifySizeMM (l :: * -> *) a where modifySizeMM = modifySizeM
class SetSizeM x a where
    setSizeM :: (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    default setSizeM :: (ModifySizeM x a) => (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    setSizeM x z = modifySizeM x (const z)
    {-# INLINE setSizeM #-}
class SetSizeMM x a where setSizeMM :: (MutMonad s m, z ~ SizeOf x) => Mut s x (Mut s a) -> z -> m ()
instance (forall b. SetSizeM l b) => SetSizeMM (l :: * -> *) a where setSizeMM = setSizeM
class EnsureSizeM x a where
    ensureSizeM :: (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    default ensureSizeM :: (GetSizeC x a, GrowSizeM x a, Num z, Ord z, MutToCstC x a) =>
        (MutMonad s m, z ~ SizeOf x) => Mut s x a -> z -> m ()
    ensureSizeM x z = do
        size <- getSizeC (cstC x)
        let diff = z - size
        when (diff > 0) $ growSizeM x diff
    {-# INLINE ensureSizeM #-}
class EnsureSizeMM x a where ensureSizeMM :: (MutMonad s m, z ~ SizeOf x) => Mut s x (Mut s a) -> z -> m ()
instance (forall b. EnsureSizeM l b) => EnsureSizeMM (l :: * -> *) a where ensureSizeMM = ensureSizeM

