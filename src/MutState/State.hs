{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilyDependencies #-}
module MutState.State (
    MutMonad,
    Mut, Cst,
    MutToCst, cst,
    MutToCstC, cstC, cstCM, cstCC,
    MutV(..),
    MutSP(..), MutSS(..),
    module Data.Primitive.MutVar,
    )
where
import Control.DeepSeq
import Data.Coerce
import Control.Monad.Primitive
import Data.Primitive.MutVar

type MutMonad s m = (PrimMonad m, s ~ PrimState m)

-- injective type families to project a pure data structure to its mutable version
type family Mut s (x :: k) = (r :: k) | r -> x s -- denotes a type that can be mutated freely
type family Cst s (x :: k) = (r :: k) | r -> x s -- denotes a type that depends on state but which is read-only

-- conversions from mutable to read-only
-- mono-kinded
class MutToCstFunc s x where
    cst :: Mut s x -> Cst s x
instance (
    Coercible (Mut s x) (Cst s x)
    ) => MutToCstFunc s x where
    cst = coerce
    {-# INLINE cst #-}
type MutToCst x = forall s. MutToCstFunc s x

-- bi-kinded
class MutToCstFuncM s (l :: * -> *) a where
    cstC :: Mut s l a -> Cst s l a
    cstCM :: Mut s l (Mut s a) -> Cst s l (Mut s a)
    cstCC :: Mut s l (Cst s a) -> Cst s l (Cst s a)
instance (
    Coercible (Mut s l) (Cst s l)
    ) => MutToCstFuncM s l a where
    cstC = coerce
    {-# INLINE cstC #-}
    cstCM = coerce
    {-# INLINE cstCM #-}
    cstCC = coerce
    {-# INLINE cstCC #-}
type MutToCstC (l :: * -> *) a = forall s. MutToCstFuncM s l a

newtype MutV x = MutV x
type instance Mut s (MutV x) = MutVar s x
type instance Cst s (MutV x) = MutVar s x

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
