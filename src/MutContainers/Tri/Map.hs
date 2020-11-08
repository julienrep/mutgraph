module MutContainers.Tri.Map (
        WriteM(..), WriteMM(..), WriteMC(..),
        ReadC(..), ReadCC(..), ReadCM(..),
        modifyM,
        ReadAt(..),
    )
where
import Prelude ((<$>))
import Control.Monad (Monad(..))
import MutState.State

class WriteM (h :: * -> * -> *) k a where
    writeM :: (MutMonad s m) => Mut s h k a -> k -> a -> m ()
class WriteMM (h :: * -> * -> *) k a where
    writeMM :: (MutMonad s m) => Mut s h k (Mut s a) -> k -> Mut s a -> m ()
instance (forall b. WriteM h k b) => WriteMM h k a where writeMM = writeM
class WriteMC (h :: * -> * -> *) k a where
    writeMC :: (MutMonad s m) => Mut s h k (Cst s a) -> k -> Cst s a -> m ()
instance (forall b. WriteM h k b) => WriteMC h k a where writeMC = writeM

class ReadC (h :: * -> * -> *) k a where
    readC :: (MutMonad s m) => Cst s h k a -> k -> m a
class ReadCC (h :: * -> * -> *) k a where
    readCC :: (MutMonad s m) => Cst s h k (Cst s a) -> k -> m (Cst s a)
instance (forall b. ReadC h k b) => ReadCC (h :: * -> * -> *) k a where readCC = readC
class ReadCM (h :: * -> * -> *) k a where
    readCM :: (MutMonad s m) => Cst s h k (Mut s a) -> k -> m (Mut s a)
instance (forall b. ReadC h k b) => ReadCM (h :: * -> * -> *) k a where readCM = readC

modifyM :: (MutMonad s m, ReadC h k a, WriteM h k a, MutToCst3 h k a) =>
    Mut s h k a -> k -> (a -> a) -> m ()
modifyM h k f = (f <$> readC (c3 h) k) >>= writeM h k
{-# INLINE modifyM #-}

class ReadAt (h :: * -> * -> *) k a where at :: h k a -> k -> a 
