module Containers.Prelude
  ( const,
    id,
    flip,
    ($),
    (.),
    (<$>),
    (!!),
    (&&),
    Foldable (null),
    Traversable (..),
    Functor (..),
    Applicative (..),
    Semigroup (..),
    Monoid (..),
    Monad (..),
    Maybe (..),
    maybe,
    Either (..),
    Num (..),
    Show (..),
    Eq (..),
    Ord (..),
    Bounded (..),
    Integral (..),
    Enum,
    Int,
    Double,
    Bool (..),
    String,
    IO,
    putStr,
    putStrLn,
    module Control.Monad,
    fromIntegral,
  )
where

import Control.Monad hiding (replicateM)
import Prelude

-- Purpose of this module is to wrap the Prelude dependency, and reexport only
-- safe non partial functions.
