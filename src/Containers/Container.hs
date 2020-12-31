module Containers.Container
  ( Convert (..),
    SizeOf,
    GetSize (..),
  )
where

import Containers.Prelude

type family SizeOf (l :: k) :: *

type instance SizeOf [] = Int

class Convert p q where convert :: p -> q

instance Convert p p where convert = id

class GetSize x where getSize :: (z ~ SizeOf x) => x -> z
