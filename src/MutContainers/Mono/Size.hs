module MutContainers.Mono.Size (
    SizeOf,
    GetSize(..),
    GetSizeC(..),
    GrowSizeM(..),
    ShrinkSizeM(..),
    ModSizeM(..),
    SetSizeM(..),
    EnsureSizeM(..),
) where
import Prelude
import Control.Monad
import MutState.State

type family SizeOf (x :: *) :: *

class GetSize x where
    getSize :: (z ~ SizeOf x) => x -> z
class GetSizeC x where
    getSizeC :: (MutMonad s m, z ~ SizeOf x) => Cst s x -> m z
class GrowSizeM x where
    growSizeM :: 
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    default growSizeM :: (ModSizeM x, Num z) =>
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    growSizeM x z = modSizeM x (+z)
    {-# INLINE growSizeM #-}
class ShrinkSizeM x where
    shrinkSizeM ::
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    default shrinkSizeM :: (ModSizeM x, Num z) =>
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    shrinkSizeM x z = modSizeM x (+(-z))
    {-# INLINE shrinkSizeM #-}
class ModSizeM x where
    modSizeM ::
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> (z -> z) -> m ()
    default modSizeM :: (GetSizeC x, GrowSizeM x, ShrinkSizeM x, Num z, Ord z, MutToCst x) => 
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> (z -> z) -> m ()
    modSizeM x f = do
        size <- getSizeC (cst x)
        let diff = (f size) - size
        if diff > 0 then growSizeM x diff
        else when (diff < 0) $ shrinkSizeM x diff
    {-# INLINE modSizeM #-}
class SetSizeM x where
    setSizeM ::
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    default setSizeM :: (ModSizeM x) => 
        (MutMonad s m, z ~ SizeOf x, Monad m) => Mut s x -> z -> m ()
    setSizeM x z = modSizeM x (const z)
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
