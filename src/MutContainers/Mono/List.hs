module MutContainers.Mono.List (
    Concat(..),
) where

class Concat l where
    concat :: l -> l -> l
