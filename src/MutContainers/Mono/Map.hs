module MutContainers.Mono.Map (
        KeyOf, ValOf,
        WriteM(..),
        ReadC(..),
        modifyM,
        ReadAt(..),
    )
where
import Prelude ((<$>))
import Control.Monad (Monad(..))
import MutState.State

type family KeyOf (l :: *) :: *
type family ValOf (l :: *) :: *

class WriteM l where
    writeM :: (MutMonad s m, k ~ KeyOf l, a ~ ValOf l) =>
        Mut s l -> k -> a -> m ()

class ReadC l where
    readC :: (MutMonad s m, k ~ KeyOf l, a ~ ValOf l) =>
        Cst s l -> k -> m a

modifyM :: (MutMonad s m, k ~ KeyOf l, a ~ ValOf l, 
    ReadC l, WriteM l, MutToCst l) =>
    Mut s l -> k -> (a -> a) -> m ()
modifyM l k f = (f <$> readC (cst l) k) >>= writeM l k
{-# INLINE modifyM #-}

class ReadAt l a where at :: (k ~ KeyOf l, a ~ ValOf l) => l -> k -> a 
