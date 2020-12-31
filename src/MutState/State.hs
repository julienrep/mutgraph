{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module MutState.State
  ( MutMonad,
    Mut,
    Cst,
    MutToCst,
    cst,
    Var (..),
    MutV (..),
    readVar,
    writeVar,
    modifyVar,
    newVar,
    RealWorld,
  )
where

import Control.Monad.Primitive (PrimMonad (..), RealWorld)
import Data.Coerce (Coercible, coerce)
import Data.Primitive.MutVar
  ( MutVar,
    modifyMutVar,
    newMutVar,
    readMutVar,
    writeMutVar,
  )
import Prelude ((<$>))

type MutMonad s m = (PrimMonad m, s ~ PrimState m)

-- injective type families are used to project a pure data structure
-- to its mutable version.
-- Only monokinded variables are supported. This design choice is deliberate
-- working with kinds gives readable sigs but cluttered and hard to read code
type family Mut s (x :: *) = (r :: *) | r -> x s -- denotes a type that can be mutated freely

type family Cst s (x :: *) = (r :: *) | r -> x s -- denotes a type that depends on state but which is read-only

class MutToCst1Func s (a :: *) where cst :: Mut s a -> Cst s a

instance (Coercible (Mut s a) (Cst s a)) => MutToCst1Func s a where
  cst = coerce
  {-# INLINE cst #-}

type MutToCst a = forall s. MutToCst1Func s a

-- below functions are used as wrapper of Data.Primitive.MutVar dependency
newtype Var x = Var x

newtype MutV s x = MutV (MutVar s x)

-- Var x denotes a type whose mutable version represents a single mutable variable of type x

type instance Mut s (Var x) = MutV s x

type instance Cst s (Var x) = MutV s x

newVar :: (MutMonad s m) => a -> m (Mut s (Var a))
newVar a = MutV <$> newMutVar a
{-# INLINE newVar #-}

readVar :: (MutMonad s m) => Mut s (Var a) -> m a
readVar (MutV v) = readMutVar v
{-# INLINE readVar #-}

writeVar :: (MutMonad s m) => Mut s (Var a) -> a -> m ()
writeVar (MutV v) = writeMutVar v
{-# INLINE writeVar #-}

modifyVar :: (MutMonad s m) => Mut s (Var a) -> (a -> a) -> m ()
modifyVar (MutV v) = modifyMutVar v
{-# INLINE modifyVar #-}
