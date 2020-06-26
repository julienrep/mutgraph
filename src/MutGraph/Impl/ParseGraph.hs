{-# OPTIONS_GHC -Wno-orphans #-}
module MutGraph.Impl.ParseGraph (
) where
import Prelude
import System.IO
import Control.DeepSeq
import Control.Exception
import MutGraph.Graph
import qualified Data.ByteString.Char8         as C

parseInt :: C.ByteString -> Int
parseInt s = let Just (n, _) = C.readInt s in n
parseHeaderNodeCount :: [C.ByteString] -> Int
parseHeaderNodeCount x = parseInt (x !! 0)
parseHeaderEdgeCount :: [C.ByteString] -> Int
parseHeaderEdgeCount x = parseInt (x !! 1)
parseEdgeTail :: [C.ByteString] -> Int
parseEdgeTail x = parseInt (x !! 0)
parseEdgeHead :: [C.ByteString] -> Int
parseEdgeHead x = parseInt (x !! 1)
parseEdgeWeight :: [C.ByteString] -> Int
parseEdgeWeight x = parseInt (x !! 2)
parseEdgeWData :: C.ByteString -> (Int, Int, Int)
parseEdgeWData line = let x = C.words line in (parseEdgeTail x, parseEdgeHead x, parseEdgeWeight x)
parseHeader :: C.ByteString -> (Int, Int)
parseHeader line = let x = C.words line in (parseHeaderNodeCount x, parseHeaderEdgeCount x)

instance ParseEdgesFromFileM [] Int Int Int where
    parseEdgesFromFileM fileName = do
        file     <- openFile fileName ReadMode
        contents <- C.hGetContents file
        let (headerLine : fileLines) = C.lines contents
        let (nodeCount, edgeCount) = parseHeader headerLine
        let edges                    = fmap parseEdgeWData fileLines
        evaluate (rnf (edges, nodeCount, edgeCount))
        hClose file
        return (edges, nodeCount, edgeCount)
    {-# INLINE parseEdgesFromFileM #-}
