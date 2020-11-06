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

class WriteM (h :: * -> * -> *) a where
    writeM :: (MutMonad s m) => Mut s h k a -> k -> a -> m ()
class WriteMM (h :: * -> * -> *) a where
    writeMM :: (MutMonad s m) => Mut s h k (Mut s a) -> k -> Mut s a -> m ()
instance (forall b. WriteM h b) => WriteMM h a where writeMM = writeM
class WriteMC (h :: * -> * -> *) a where
    writeMC :: (MutMonad s m) => Mut s h k (Cst s a) -> k -> Cst s a -> m ()
instance (forall b. WriteM h b) => WriteMC h a where writeMC = writeM

class ReadC (h :: * -> * -> *) a where
    readC :: (MutMonad s m) => Cst s h k a -> k -> m a
class ReadCC (h :: * -> * -> *) a where
    readCC :: (MutMonad s m) => Cst s h k (Cst s a) -> k -> m (Cst s a)
instance (forall b. ReadC h b) => ReadCC (h :: * -> * -> *) a where readCC = readC
class ReadCM (h :: * -> * -> *) a where
    readCM :: (MutMonad s m) => Cst s h k (Mut s a) -> k -> m (Mut s a)
instance (forall b. ReadC h b) => ReadCM (h :: * -> * -> *) a where readCM = readC

modifyM :: (MutMonad s m, ReadC h a, WriteM h a, MutToCst3 h k a) =>
    Mut s h k a -> k -> (a -> a) -> m ()
modifyM h k f = (f <$> readC (c3 h) k) >>= writeM h k
{-# INLINE modifyM #-}

class ReadAt (h :: * -> * -> *) a where at :: h k a -> k -> a 
