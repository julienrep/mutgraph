{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module Tests.Sandbox (
    tests,
)
where
import Prelude hiding (map, replicate)
import Test.HUnit ( assertEqual, Test(..) )
import MutContainers.Vector
import MutContainers.Bi.Container
import MutContainers.Bi.List hiding (enumFromTo)
import MutContainers.Bi.Map
import MutContainers.Bi.Size
import MutState.State
import Control.Monad hiding (replicateM)
import MutContainers.Any.Map
import MutContainers.Any.Size

dputs :: String -> IO ()
dputs = putStr
lr :: String
lr = "\n"

tests :: Test
tests = TestList [
        TestLabel "test1" test1
        ]

test1 :: Test
test1 = TestCase $ do
    dputs lr
    dputs $ "Hello world" ++ lr

    l :: Mut _ Vector Int <- replicateM 5 (return 3)
    ll :: Mut _ Vector (Mut _ Vector Int) <- replicateM 2 (return l)

    dputs $ "l = "
    listToStringM (return . show) l >>= dputs
    dputs $ lr
 
    dputs $ "ll = "
    listToStringM (listToStringM (return . show)) ll >>= dputs
    dputs $ lr

    assertEqual "Check results" (1 :: Int) (1 :: Int)

listToStringM :: (MutMonad s m, 
    GetSizeC l a, ReadC l a, SizeOf l ~ Int, KeyOf l ~ Int) => 
    (a -> m String) -> Cst s l a -> m String
listToStringM f l = do
    n <- getSizeC l
    r <- forM (enumFromTo 0 (n-1)) (\i -> do 
            x <- readC l i
            sx <- f x
            return $ sx ++ (if i < (n-1) then ", " else "]")
        )
    foldM ((return .) . (++)) "[" r
