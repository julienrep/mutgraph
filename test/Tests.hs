module Tests (
    main
)
where
import System.IO
import Test.HUnit
import qualified Tests.MutGraph.Dijkstra
-- import qualified Tests.Sandbox

tests :: Test
tests = TestList [
        TestLabel "Tests.MutGraph.Dijkstra" Tests.MutGraph.Dijkstra.tests
        -- TestLabel "Tests.Sandbox" Tests.Sandbox.tests
        ]

main :: IO Counts
main = runTestTT tests

