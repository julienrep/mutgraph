module MutContainers.Any.Map (
        KeyOf, ValOf,
    )
where

type family KeyOf (l :: k) :: *
type family ValOf (l :: k) :: *
