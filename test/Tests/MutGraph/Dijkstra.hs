{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# LANGUAGE TemplateHaskell #-}
module Tests.MutGraph.Dijkstra (
    tests,
)
where
import Prelude hiding (map)
import Test.HUnit
import System.CPUTime
import System.Mem
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import MutState.State
import MutGraph.Graph
import MutGraph.AdjacencyList
import MutGraph.Dijkstra
import MutGraph.Parser
import MutContainers.Curry
import MutContainers.Run
import MutContainers.Vector
import MutContainers.Unbox
import MutContainers.List

dputs :: String -> IO ()
dputs = putStr
lr :: String
lr = "\n"

tests :: Test
tests = TestList [
        TestLabel "testDijkstraFromList" testDijkstraFromList, 
        TestLabel "testDijkstraFromFile" testDijkstraFromFile,
        TestLabel "test1" test1
        ]

type AdjGraphU e = AdjLst (Vector Int) Int DVectorM VectorU Int e
type AdjGraph e = AdjLst (Vector Int) Int DVectorM Vector Int e

dijkstraFormatInputsM :: (MutMonad s IO, Foldable list) => 
    (list (Int, Int, Int), Int)
    -> IO (Mut s (AdjGraphU Int), Int)
dijkstraFormatInputsM (list, source) = do
    graph <- makeGraphFromEdgesM list
    let inputs = (graph, source)
    return inputs
dijkstraRunM :: (MutMonad s IO) => 
    (Mut s (AdjGraphU Int), Int)
    -> IO (Vector Int (Int, Int))
dijkstraRunM inputs = do
    evaluate (rnf inputs)
    performGC
    dputs $ "Dijkstra running on graph..." ++ lr
    t1 <- getCPUTime
    outputs <- uncurryN dijkstraSimpleM inputs
    evaluate (rnf outputs)
    t2 <- getCPUTime
    let dt :: Double = fromIntegral (t2-t1) * 1e-12
    dputs $ "Dijkstra ran in " ++ show dt ++ " s" ++ lr
    return outputs
dijkstraFormatOutputsM :: Vector Int (Int, Int) -> IO [Int]
dijkstraFormatOutputsM = return . map (\(_, x) -> if x < maxBound then x else 0) . toList

testDijkstraFromFile :: Test
testDijkstraFromFile = TestCase $ do
    dputs lr
    x <- parseEdgesFromFileM (SimpleParser "../dijkstra2.txt")
    case x of
        Nothing -> dputs $ "File could not be parsed!" ++ lr
        Just r -> do
            dputs $ "File parsed!" ++ lr
            let (input :: [] (Int, Int, Int), _ :: Int, _ :: Int) = r
            let source = 1
            _ <- runM dijkstraFormatInputsM dijkstraRunM dijkstraFormatOutputsM (input, source)
            return ()

testDijkstraFromList :: Test
testDijkstraFromList = TestCase $ do
    dputs lr
    let input :: [(Int, Int, Int)] =
            [
            (1, 2, 7), (1, 3, 9), (1, 6, 14)
            , (2, 1, 7), (2, 3, 10), (2, 4, 15)
            , (3, 1, 9), (3, 2, 10), (3, 4, 11), (3, 6, 2)
            , (4, 2, 15), (4, 3, 11), (4, 5, 6)
            , (5, 4, 6), (5, 6, 9)
            , (6, 1, 14), (6, 3, 2), (6, 5, 9)
            ]
    let source = 1
    output <- runM dijkstraFormatInputsM dijkstraRunM dijkstraFormatOutputsM (input, source)
    assertEqual "Check results" output [0 :: Int, 0, 7, 9, 20, 20, 11]


test1 :: Test
test1 = TestCase $ do
    dputs lr
    let input :: [(Int, Int, Int)] =
            [
            (1, 2, 7), (1, 3, 9), (1, 6, 14)
            , (2, 1, 7), (2, 3, 10), (2, 4, 15)
            , (3, 1, 9), (3, 2, 10), (3, 4, 11), (3, 6, 2)
            , (4, 2, 15), (4, 3, 11), (4, 5, 6)
            , (5, 4, 6), (5, 6, 9)
            , (6, 1, 14), (6, 3, 2), (6, 5, 9)
            ]
    -- input2 <- forM input $ \(u,v,e) -> do
    --         x <- newMutVar e
    --         return (u,v,x)
    let input2 = fmap (\(a, b, c) -> (a, b, AAA c)) input
    -- mgraph :: Mut RealWorld (AdjList VectorU Int DVector VectorU Int Int) <- makeGraphFromEdgesM input
    -- listGraphEdgesC mgraph >>=
    --     mapM_ (\edge -> do
    --         let h = getEdgeKey edge
    --         modGraphEdgeM mgraph (*10) h
    --         )

    mgraph :: Mut RealWorld (AdjGraphU (AAA Int)) <- makeGraphFromEdgesM input2
    listGraphEdgesC mgraph >>=
        mapM_ (\(Edge (u, v, AAA e, h)) -> do
            dputs $ "(" ++ show u
            dputs $ ", " ++ show v
            dputs $ ", " ++ show h
            dputs $ ", " ++ show e
            dputs $ ")" ++ lr
            )
    vout :: Vector Int (Int, Int) <- dijkstraSimpleM (fmapGraph (\(AAA a) -> a) mgraph) 1
    dputs $ "output: " ++ show (toList vout) ++ lr
    return ()

newtype AAA a = AAA a

derivingUnbox "AAA"
    [t| forall a. (Unbox a) => AAA a -> a |]
    [| \ (AAA a) -> a |]
    [| AAA |]
