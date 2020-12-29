{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tests.Sandbox
  ( tests,
  )
where

import Containers.Container
import Containers.List
import Containers.Prelude
import MutContainers.Container
import MutContainers.List
import MutContainers.Map
import MutContainers.Vector
import MutState.State
import Test.HUnit (Test (..), assertEqual)

dputs :: String -> IO ()
dputs = putStr

lr :: String
lr = "\n"

tests :: Test
tests =
  TestList
    [ TestLabel "test1" test1
    ]

test1 :: Test
test1 = TestCase $ do
  dputs lr
  dputs $ "Hello world" ++ lr
  l :: Mut _ (Vector Int Int) <- replicateM 5 (return 3)
  ll :: Mut _ (Vector Int (Mut _ (Vector Int Int))) <- replicateM 2 (return l)
  dputs "ll = "
  listToStringM (listToStringM (return . show)) ll >>= dputs
  dputs lr

listToStringM ::
  ( MutMonad s m,
    GetSizeC (l a),
    SizeOf (l a) ~ Int,
    ReadC (l a),
    ValOf (l a) ~ a,
    KeyOf (l a) ~ Int
  ) =>
  (a -> m String) ->
  Cst s (l a) ->
  m String
listToStringM f l = do
  n <- getSizeC l
  let g = \i -> do
        x <- readC l i
        sx <- f x
        return $ sx ++ (if i < (n -1) then ", " else "]")
  let h :: [] Int = enumFromTo 0 (n - 1)
  r <- forM h g
  foldM ((return .) . (++)) "[" r
