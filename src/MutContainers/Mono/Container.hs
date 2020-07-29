module MutContainers.Mono.Container (
    MakeNew(..),
    Convert(..),
    ConvertM(..),
    UConvertM(..),
    FreezeC(..),
    UFreezeC(..),
    ThawM(..),
    UThawM(..),
    IsEmptyC(..),
    EmptyM(..),
    MakeNewM(..),
) where
import Prelude
import MutState.State

class MakeNew l where
    makeNew :: l
class Convert p q where
    convert :: p -> q

class ConvertM p q where
    convertM :: (MutMonad s m) => Mut s p -> m (Mut s q)
class UConvertM p q where
    uconvertM :: (MutMonad s m) => Mut s p -> m (Mut s q)
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
class MakeNewM l where
    makeNewM :: (MutMonad s m) => m (Mut s l)
