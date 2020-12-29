{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Containers.NonEmpty
  ( NonEmpty (..),
    toNonEmpty,
    fromNonEmpty,
    NonEmpty1 (..),
  )
where

import Containers.Prelude


newtype NonEmpty l a = NonEmpty (l a)

deriving instance (Functor l) => Functor (NonEmpty l)

toNonEmpty :: Foldable l => l a -> Maybe (NonEmpty l a)
toNonEmpty x = if null x then Nothing else Just (NonEmpty x)

fromNonEmpty :: NonEmpty x a -> x a
fromNonEmpty (NonEmpty l) = l


-- newtype LL l e = LL l
-- type NonEmpty1 l = NonEmpty (LL l) (ValOf l)
newtype NonEmpty1 x = NonEmpty1 x
