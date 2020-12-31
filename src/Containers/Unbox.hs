module Containers.Unbox (
    Unbox,
    module Data.Vector.Unboxed.Deriving,
)
where
import qualified Data.Vector.Unboxed           as VU
import Data.Vector.Unboxed.Deriving
type Unbox = VU.Unbox
