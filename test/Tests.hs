module Tests (
    main
)
where
import System.IO
import Test.HUnit
import qualified Tests.MutGraph.Dijkstra

tests :: Test
tests = TestList [
        TestLabel "Tests.MutGraph.Dijkstra" Tests.MutGraph.Dijkstra.tests
        ]

main :: IO Counts
main = runTestTT tests

