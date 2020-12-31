module Containers.Prelude
  ( 
    const,
    id,
    flip,
    ($),
    (.),
    (<$>),
    Foldable (null),
    Traversable (..),
    Functor (..),
    Applicative (..),
    Semigroup (..),
    Monoid (..),
    Monad (..),
    Maybe (..),
    maybe,
    Eq (..),
    Ord (..),
    Enum,
    Int,
    Bool,
    Num(..),
    String,
    IO,
    Show(..),
    putStr, putStrLn,
    module Control.Monad,
  )
where

import Prelude
import Control.Monad hiding (replicateM)

-- Purpose of this module is to wrap the Prelude dependency, and reexport only
-- safe non partial functions
