{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module MutState.State (
    -- module Data.Primitive.MutVar,
    MutMonad,
    Mut, Cst,
    MutToCst, cst,
    MutToCst2, c2, c2C, c2M, m1m2, c1c2, m2m1, c2c1, c1m2, c2m1,
    MutToCst3, c3, m1m3, m2m3, m3m2, m3m1, c1c3, c2c3, c3c2, c3c1, c1m3, c2m3, c3m2, c3m1,
    MutV(..),
    MutSP(..), MutSS(..),
    readMutV, writeMutV, modifyMutV, newMutV,
    )
where
import Control.DeepSeq
import Data.Coerce
import Control.Monad.Primitive
import Data.Primitive.MutVar

type MutMonad s m = (PrimMonad m, s ~ PrimState m)

-- injective type families to project a pure data structure to its mutable version
-- type family Mut s (x :: k) = (r :: k) | r -> x s -- denotes a type that can be mutated freely
-- type family Cst s (x :: k) = (r :: k) | r -> x s -- denotes a type that depends on state but which is read-only

newtype MutV x = MutV x
type instance Mut s (MutV x) = MutVar s x
type instance Cst s (MutV x) = MutVar s x


type family Mut s (x :: k) = (r :: k) | r -> x s
type family Cst s (x :: k) = (r :: k) | r -> x s

class MutToCst1Func s (a :: *) where cst :: Mut s a -> Cst s a
instance (Coercible (Mut s a) (Cst s a)) => MutToCst1Func s a where
    cst = coerce
    {-# INLINE cst #-}
type MutToCst a = forall s. MutToCst1Func s a

class MutToCstFunc2 s (l :: * -> *) (a :: *) where
    c2 :: Mut s l a -> Cst s l a
    c2M :: Mut s l (Mut s a) -> Cst s l (Mut s a)
    c2C :: Mut s l (Cst s a) -> Cst s l (Cst s a)
    m1m2 :: Mut s l a -> Mut s (l a)
    c1c2 :: Cst s l a -> Cst s (l a)
    m2m1 :: Mut s (l a) -> Mut s l a
    c2c1 :: Cst s (l a) -> Cst s l a
    c1m2 :: Mut s l a -> Cst s (l a)
    c2m1 :: Mut s (l a) -> Cst s l a
instance (
    Coercible (Mut s l) (Cst s l),
    Coercible (Mut s l a) (Mut s (l a)),
    Coercible (Cst s l a) (Cst s (l a))
    ) => MutToCstFunc2 s l a where
    c2 = coerce
    {-# INLINE c2 #-}
    c2M = coerce
    {-# INLINE c2M #-}
    c2C = coerce
    {-# INLINE c2C #-}
    m1m2 = coerce
    {-# INLINE m1m2 #-}
    c1c2 = coerce
    {-# INLINE c1c2 #-}
    m2m1 = coerce
    {-# INLINE m2m1 #-}
    c2c1 = coerce
    {-# INLINE c2c1 #-}
    c1m2 = coerce
    {-# INLINE c1m2 #-}
    c2m1 = coerce
    {-# INLINE c2m1 #-}
type MutToCst2 (l :: * -> *) a = forall s. MutToCstFunc2 s l a

class MutToCstFunc3 s (h :: * -> * -> *) (k :: *) (a :: *) where
    c3 :: Mut s h k a -> Cst s h k a
    m1m3 :: Mut s h k a -> Mut s (h k a)
    m2m3 :: Mut s h k a -> Mut s (h k) a
    m3m2 :: Mut s (h k) a -> Mut s h k a
    m3m1 :: Mut s (h k a) -> Mut s h k a
    c1c3 :: Cst s h k a -> Cst s (h k a)
    c2c3 :: Cst s h k a -> Cst s (h k) a
    c3c2 :: Cst s (h k) a -> Cst s h k a
    c3c1 :: Cst s (h k a) -> Cst s h k a
    c1m3 :: Mut s h k a -> Cst s (h k a)
    c2m3 :: Mut s h k a -> Cst s (h k) a
    c3m2 :: Mut s (h k) a -> Cst s h k a
    c3m1 :: Mut s (h k a) -> Cst s h k a
instance (
    Coercible (Mut s h) (Cst s h),
    Coercible (Mut s h k a) (Mut s (h k a)),
    Coercible (Mut s h k) (Mut s (h k)),
    Coercible (Cst s h k a) (Cst s (h k a)),
    Coercible (Cst s h k) (Cst s (h k))
    ) => MutToCstFunc3 s h k a where
    c3 = coerce
    {-# INLINE c3 #-}
    m1m3 = coerce
    {-# INLINE m1m3 #-}
    m2m3 = coerce
    {-# INLINE m2m3 #-}
    m3m2 = coerce
    {-# INLINE m3m2 #-}
    m3m1 = coerce
    {-# INLINE m3m1 #-}
    c1c3 = coerce
    {-# INLINE c1c3 #-}
    c2c3 = coerce
    {-# INLINE c2c3 #-}
    c3c2 = coerce
    {-# INLINE c3c2 #-}
    c3c1 = coerce
    {-# INLINE c3c1 #-}
    c1m3 x = c1c3 (c3 x)
    {-# INLINE c1m3 #-}
    c2m3 x = c2c3 (c3 x)
    {-# inline c2m3 #-}
    c3m2 x = c3 (m3m2 x)
    {-# inline c3m2 #-}
    c3m1 x = c3 (m3m1 x)
    {-# inline c3m1 #-}
type MutToCst3 (h :: * -> * -> *) k a = forall s. MutToCstFunc3 s h k a


-- tricks to allow using nested state newtypes without explicit 's'
newtype MutSP l a = MutSP (l a) -- this is for a * -> * that depends on State but elements are Pure
newtype MutSS l a = MutSS (l a) -- this is for a * -> * that depends on State and elements depend on State
instance (NFData (l a)) => NFData (MutSP l a) where
  rnf (MutSP l) = rnf l
  {-# INLINE rnf #-}
instance (NFData (l a)) => NFData (MutSS l a) where
  rnf (MutSS l) = rnf l
  {-# INLINE rnf #-}
type instance Mut s (MutSP l a) = MutSP (Mut s l) a
type instance Mut s (MutSS l a) = MutSS (Mut s l) (Mut s a)
type instance Cst s (MutSP l a) = MutSP (Cst s l) a
type instance Cst s (MutSS l a) = MutSS (Cst s l) (Cst s a)
type instance Mut s (MutSP l) = MutSP (Mut s l)
type instance Cst s (MutSP l) = MutSP (Cst s l)


newMutV :: (MutMonad s m) => a -> m (Mut s (MutV a))
newMutV = newMutVar
{-# INLINE newMutV #-}
readMutV :: (MutMonad s m) => Mut s (MutV a) -> m a
readMutV = readMutVar
{-# INLINE readMutV #-}
writeMutV :: (MutMonad s m) => Mut s (MutV a) -> a -> m ()
writeMutV = writeMutVar
{-# INLINE writeMutV #-}
modifyMutV :: (MutMonad s m) => Mut s (MutV a) -> (a -> a) -> m ()
modifyMutV = modifyMutVar
{-# INLINE modifyMutV #-}
