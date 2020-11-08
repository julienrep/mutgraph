module MutContainers.Mono.Map (
        WriteM(..),
        WriteMM(..),
        WriteMC(..),
        ReadC(..),
        ReadCC(..),
        ReadMM(..),
        modifyM,
        ReadAt(..),
    )
where
import Prelude ((<$>))
import Control.Monad (Monad(..))
import MutState.State
import MutContainers.Any.Map

type instance ValOf [a] = a

class WriteM h where
    writeM :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Mut s h -> k -> a -> m ()
class WriteMM h where
    writeMM :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Mut s h -> k -> Mut s a -> m ()
class WriteMC h where
    writeMC :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Mut s h -> k -> Cst s a -> m ()

class ReadC h where
    readC :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Cst s h -> k -> m a
class ReadCC h where
    readCC :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Cst s h -> k -> m (Cst s a)
class ReadMM h where
    readMM :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Mut s h -> k -> m (Mut s a)

modifyM :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h, 
    ReadC h, WriteM h, MutToCst h) =>
    Mut s h -> k -> (a -> a) -> m ()
modifyM h k f = (f <$> readC (cst h) k) >>= writeM h k
{-# INLINE modifyM #-}

class ReadAt h where at :: (k ~ KeyOf h, a ~ ValOf h) => h -> k -> a 
