{-# LANGUAGE TemplateHaskell #-}
module MutContainers.Bi.Container (
        MakeNew(..),

        Convert(..),
        ConvertM(..), ConvertMM(..),
        UConvertM(..), UConvertMM(..),
        FreezeC(..), FreezeCC(..),
        UFreezeC(..), UFreezeCC(..),
        ThawM(..), ThawMM(..),
        UThawM(..), UThawMM(..),
        IsEmptyC(..), IsEmptyCC(..),
        EmptyM(..), EmptyMM(..),
        MakeNewM(..), MakeNewMM(..),
    )
where
import Prelude hiding (length, zip, map, enumFromTo)
import MutState.State

class Convert (p :: * -> *) (q :: * -> *) a where convert :: p a -> q a
class MakeNew (l :: * -> *) a where makeNew :: l a


class ConvertM (p :: * -> *) (q :: * -> *) a where convertM :: (MutMonad s m) => Mut s p a -> m (Mut s q a)
class ConvertMM (p :: * -> *) (q :: * -> *) a where convertMM :: (MutMonad s m) => Mut s p (Mut s a) -> m (Mut s q (Mut s a))
-- needs specific derivation
class UConvertM (p :: * -> *) (q :: * -> *) a where uconvertM :: (MutMonad s m) => Mut s p a -> m (Mut s q a)
class UConvertMM (p :: * -> *) (q :: * -> *) a where uconvertMM :: (MutMonad s m) => Mut s p (Mut s a) -> m (Mut s q (Mut s a))
-- needs specific derivation
class FreezeC (l :: * -> *) a where freezeC :: (MutMonad s m) => Cst s l a -> m (l a)
class FreezeCC (l :: * -> *) a where freezeCC :: (MutMonad s m) => Cst s l (Cst s a) -> m (l (Cst s a))
instance (forall b. FreezeC l b) => FreezeCC (l :: * -> *) a where freezeCC = freezeC
class UFreezeC (l :: * -> *) a where ufreezeC :: (MutMonad s m) => Cst s l a -> m (l a)
class UFreezeCC (l :: * -> *) a where ufreezeCC :: (MutMonad s m) => Cst s l (Cst s a) -> m (l (Cst s a))
instance (forall b. UFreezeC l b) => UFreezeCC (l :: * -> *) a where ufreezeCC = ufreezeC
class ThawM (l :: * -> *) a where thawM :: (MutMonad s m) => l a -> m (Mut s l a)
class ThawMM (l :: * -> *) a where thawMM :: (MutMonad s m) => l (Mut s a) -> m (Mut s l (Mut s a))
instance (forall b. ThawM l b) => ThawMM (l :: * -> *) a where thawMM = thawM
class UThawM (l :: * -> *) a where uthawM :: (MutMonad s m) => l a -> m (Mut s l a)
class UThawMM (l :: * -> *) a where uthawMM :: (MutMonad s m) => l (Mut s a) -> m (Mut s l (Mut s a))
instance (forall b. UThawM l b) => UThawMM (l :: * -> *) a where uthawMM = uthawM
class IsEmptyC (l :: * -> *) a where isEmptyC :: (MutMonad s m) => Cst s l a -> m Bool
class IsEmptyCC (l :: * -> *) a where isEmptyCC :: (MutMonad s m) => Cst s l (Cst s a) -> m Bool
instance (forall b. IsEmptyC l b) => IsEmptyCC (l :: * -> *) a where isEmptyCC = isEmptyC
class EmptyM (l :: * -> *) a where emptyM :: (MutMonad s m) => Mut s l a -> m ()
class EmptyMM (l :: * -> *) a where emptyMM :: (MutMonad s m) => Mut s l (Mut s a) -> m ()
instance (forall b. EmptyM l b) => EmptyMM (l :: * -> *) a where emptyMM = emptyM
class MakeNewM (l :: * -> *) a where makeNewM :: (MutMonad s m) => m (Mut s l a)
class MakeNewMM (l :: * -> *) a where makeNewMM :: (MutMonad s m) => m (Mut s l (Mut s a))
instance (forall b. MakeNewM l b) => MakeNewMM (l :: * -> *) a where makeNewMM = makeNewM
