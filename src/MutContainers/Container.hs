module MutContainers.Container (
    MakeNewM(..),
    FreezeC(..),
    UFreezeC(..),
    ThawM(..),
    UThawM(..),
    IsEmptyC(..),
    EmptyM(..),
    GetSizeC(..),
    GrowSizeM(..),
    ShrinkSizeM(..),
    ModifySizeM(..),
    SetSizeM(..),
    EnsureSizeM(..),
) where
import Containers.Prelude
import Containers.Container ( SizeOf )
import MutState.State


class MakeNewM l where
    makeNewM :: (MutMonad s m) => m (Mut s l)
class FreezeC l where
    freezeC :: (MutMonad s m) => Cst s l -> m l
class UFreezeC l where
    ufreezeC :: (MutMonad s m) => Cst s l -> m l
class ThawM l where
    thawM :: (MutMonad s m) => l -> m (Mut s l)
class UThawM l where
    uthawM :: (MutMonad s m) => l -> m (Mut s l)
class IsEmptyC l where
    isEmptyC :: (MutMonad s m) => Cst s l -> m Bool
class EmptyM l where
    emptyM :: (MutMonad s m) => Mut s l -> m ()

class GetSizeC x where
    getSizeC :: (MutMonad s m, z ~ SizeOf x) => Cst s x -> m z
class GrowSizeM x where
    growSizeM :: 
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    default growSizeM :: (ModifySizeM x, Num z) =>
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    growSizeM x z = modifySizeM x (+z)
    {-# INLINE growSizeM #-}
class ShrinkSizeM x where
    shrinkSizeM ::
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    default shrinkSizeM :: (ModifySizeM x, Num z) =>
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    shrinkSizeM x z = modifySizeM x (+(-z))
    {-# INLINE shrinkSizeM #-}
class ModifySizeM x where
    modifySizeM ::
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> (z -> z) -> m ()
    default modifySizeM :: (GetSizeC x, GrowSizeM x, ShrinkSizeM x, Num z, Ord z, MutToCst x) => 
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> (z -> z) -> m ()
    modifySizeM x f = do
        size <- getSizeC (cst x)
        let diff = f size - size
        if diff > 0 then growSizeM x diff
        else when (diff < 0) $ shrinkSizeM x (-diff)
    {-# INLINE modifySizeM #-}
class SetSizeM x where
    setSizeM ::
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    default setSizeM :: (ModifySizeM x) => 
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    setSizeM x z = modifySizeM x (const z)
    {-# INLINE setSizeM #-}
class EnsureSizeM x where
    ensureSizeM ::
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    default ensureSizeM :: (GetSizeC x, GrowSizeM x, Num z, Ord z, MutToCst x) => 
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    ensureSizeM x z = do
        size <- getSizeC (cst x)
        let diff = z - size
        when (diff > 0) $ growSizeM x diff
    {-# INLINE ensureSizeM #-}
