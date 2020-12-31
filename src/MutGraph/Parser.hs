module MutGraph.Parser
  ( SimpleParser (..),
  )
where

-- import Control.DeepSeq
import Containers.Container
import Containers.List
import Containers.NonEmpty
import Containers.Prelude
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as C
import MutContainers.Map
import MutGraph.Graph
import System.IO

parseInt :: C.ByteString -> Int
parseInt s = let Just (n, _) = C.readInt s in n

parseHeaderNodeCount :: (ReadAt h, Num (KeyOf h), ValOf h ~ C.ByteString) => h -> Int
parseHeaderNodeCount x = parseInt (x `at` 0)

parseHeaderEdgeCount :: (ReadAt h, Num (KeyOf h), ValOf h ~ C.ByteString) => h -> Int
parseHeaderEdgeCount x = parseInt (x `at` 1)

parseEdgeTail :: (ReadAt h, Num (KeyOf h), ValOf h ~ C.ByteString) => h -> Int
parseEdgeTail x = parseInt (x `at` 0)

parseEdgeHead :: (ReadAt h, Num (KeyOf h), ValOf h ~ C.ByteString) => h -> Int
parseEdgeHead x = parseInt (x `at` 1)

parseEdgeWeight :: (ReadAt h, Num (KeyOf h), ValOf h ~ C.ByteString) => h -> Int
parseEdgeWeight x = parseInt (x `at` 2)

parseEdgeWData :: C.ByteString -> (Int, Int, Int)
parseEdgeWData line = (parseEdgeTail x, parseEdgeHead x, parseEdgeWeight x)
  where
    x = C.words line

parseHeader :: C.ByteString -> (Int, Int)
parseHeader line = (parseHeaderNodeCount x, parseHeaderEdgeCount x)
  where
    x = C.words line

newtype SimpleParser = SimpleParser String

instance
  () =>
  -- Head l,
  -- Tail l,
  -- Empty l,
  -- Functor l,
  -- Convert [C.ByteString] (l C.ByteString)

  ParseEdgesFromFileM SimpleParser [] Int Int Int
  where
  parseEdgesFromFileM (SimpleParser fileName) = do
    x <- try parse
    case x of
      Left (_ :: SomeException) -> return Nothing
      Right r -> return (Just r)
    where
      parse = do
        file <- openFile fileName ReadMode
        contents <- C.hGetContents file
        let lines = convert (C.lines contents)
        case toNonEmpty lines of
          Nothing -> return (empty, 0, 0)
          Just nelines -> do
            let headerLine = head nelines
            let fileLines = tail nelines
            let (nodeCount, edgeCount) = parseHeader headerLine
            let edges = fmap parseEdgeWData fileLines
            -- evaluate (rnf (edges, nodeCount, edgeCount))
            -- hClose file
            return (edges, nodeCount, edgeCount)
  {-# INLINE parseEdgesFromFileM #-}
