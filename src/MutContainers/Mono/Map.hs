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

type family KeyOf (h :: *) :: *
type family ValOf (h :: *) :: *
type instance ValOf [a] = a

class WriteM h where
    writeM :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Mut s h -> k -> a -> m ()

class ReadC h where
    readC :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h) =>
        Cst s h -> k -> m a

modifyM :: (MutMonad s m, k ~ KeyOf h, a ~ ValOf h, 
    ReadC h, WriteM h, MutToCst h) =>
    Mut s h -> k -> (a -> a) -> m ()
modifyM h k f = (f <$> readC (cst h) k) >>= writeM h k
{-# INLINE modifyM #-}

class ReadAt h a where at :: (k ~ KeyOf h, a ~ ValOf h) => h -> k -> a 
