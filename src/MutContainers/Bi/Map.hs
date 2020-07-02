module MutContainers.Bi.Map (
        KeyOf,
        WriteM(..), WriteMM(..), WriteMC(..),
        ReadC(..), ReadCC(..), ReadCM(..),
        modifyM,
        ReadAt(..),
    )
where
import Prelude ((<$>))
import Control.Monad (Monad(..))
import MutState.State

type family KeyOf (l :: * -> *) :: *

class WriteM (l :: * -> *) a where
    writeM :: (MutMonad s m, k ~ KeyOf l) =>
        Mut s l a -> k -> a -> m ()
class WriteMM (l :: * -> *) a where
    writeMM :: (MutMonad s m, k ~ KeyOf l) => 
        Mut s l (Mut s a) -> k -> Mut s a -> m ()
instance (forall b. WriteM l b) => WriteMM l a where writeMM = writeM
class WriteMC (l :: * -> *) a where
    writeMC :: (MutMonad s m, k ~ KeyOf l) =>
        Mut s l (Cst s a) -> k -> Cst s a -> m ()
instance (forall b. WriteM l b) => WriteMC l a where writeMC = writeM

class ReadC (l :: * -> *) a where
    readC :: (MutMonad s m, k ~ KeyOf l) =>
        Cst s l a -> k -> m a
class ReadCC (l :: * -> *) a where
    readCC :: (MutMonad s m, k ~ KeyOf l) =>
        Cst s l (Cst s a) -> k -> m (Cst s a)
instance (forall b. ReadC l b) => ReadCC (l :: * -> *) a where readCC = readC
class ReadCM (l :: * -> *) a where
    readCM :: (MutMonad s m, k ~ KeyOf l) =>
        Cst s l (Mut s a) -> k -> m (Mut s a)
instance (forall b. ReadC l b) => ReadCM (l :: * -> *) a where readCM = readC

modifyM :: (MutMonad s m, k ~ KeyOf l, ReadC l a, WriteM l a, MutToCstC l a) =>
    Mut s l a -> k -> (a -> a) -> m ()
modifyM l k f = (f <$> readC (cstC l) k) >>= writeM l k
{-# INLINE modifyM #-}

class ReadAt (l :: * -> *) a where at :: (k ~ KeyOf l) => l a -> k -> a 
