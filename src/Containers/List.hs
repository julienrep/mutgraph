{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Containers.List
  ( Head (..),
    Last (..),
    Tail (..),
    Empty (..),
    Singleton (..),
    Cons (..),
    Snoc (..),
    Zip (..),
    ZipWith (..),
    EnumFrom (..),
    EnumFromTo (..),
    Append (..),
    Replicate (..),
    ToList (..),
    FromList (..),
  )
where

import Containers.NonEmpty
import Containers.Prelude as P
import Prelude as P (enumFrom, enumFromTo)
import qualified Data.List as L
import Containers.Container (SizeOf)

-- | Extract the first element of a non-empty list.
-- Time : constant in size(list)
-- Space : constant in size(list)
class Head l where head :: NonEmpty l a -> a

-- | Extract the last element of a non-empty list.
-- Time : constant in size(list)
-- Space : constant in size(list)
class Last l where last :: NonEmpty l a -> a

-- | Extract the elements after the head of a non-empty list.
-- Time : constant in size(list)
-- Space : constant in size(list)
class Tail l where tail :: NonEmpty l a -> l a

class Empty l where empty :: l a

class Singleton l where singleton :: a -> l a

deriving instance (Singleton l) => Singleton (NonEmpty l)

class Cons l where cons :: a -> l a -> l a

deriving instance (Cons l) => Cons (NonEmpty l)

class Snoc l where snoc :: l a -> a -> l a

deriving instance (Snoc l) => Snoc (NonEmpty l)

class Zip l where zip :: l a -> l b -> l (a, b)

deriving instance (Zip l) => Zip (NonEmpty l)

class ZipWith l where zipWith :: (a -> b -> c) -> l a -> l b -> l c

deriving instance (ZipWith l) => ZipWith (NonEmpty l)

class EnumFrom l where enumFrom :: Enum a => a -> l a

class EnumFromTo l where enumFromTo :: Enum a => a -> a -> l a

class Append l where (++) :: l a -> l a -> l a

deriving instance (Append l) => Append (NonEmpty l)

class Replicate l where replicate :: SizeOf l -> a -> l a

class ToList l where toList :: l a -> [a]

class FromList l where fromList :: [a] -> l a

-- instances
instance Head [] where head = L.head . fromNonEmpty

-- instance Last [] is not defined because P.last is O(n) where n=size(list)

instance Tail [] where tail = L.tail . fromNonEmpty

instance Empty [] where empty = []

instance Singleton [] where singleton a = [a]

instance Cons [] where cons a l = a : l

instance Snoc [] where snoc l a = l ++ [a]

instance Zip [] where zip = L.zip

instance ZipWith [] where zipWith = L.zipWith

instance EnumFrom [] where enumFrom = P.enumFrom

instance EnumFromTo [] where enumFromTo = P.enumFromTo

instance Append [] where l1 ++ l2 = l1 L.++ l2

instance Replicate [] where replicate = L.replicate

instance ToList [] where toList = id

instance FromList [] where fromList = id
