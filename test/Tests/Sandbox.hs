{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Tests.Sandbox
  ( tests,
  )
where

import Control.Monad hiding (replicateM)
import MutContainers.Container
import MutContainers.List hiding (enumFromTo)
import MutContainers.Map
import MutContainers.Size
import MutContainers.Vector
import MutState.State
import Test.HUnit (Test (..), assertEqual)
import Prelude hiding (map, replicate)

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
  r <-
    forM
      (enumFromTo 0 (n -1))
      ( \i -> do
          x <- readC l i
          sx <- f x
          return $ sx ++ (if i < (n -1) then ", " else "]")
      )
  foldM ((return .) . (++)) "[" r
