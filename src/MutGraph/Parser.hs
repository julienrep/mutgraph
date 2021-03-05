module MutGraph.Parser
  ( SimpleParser (..),
  )
where

import Containers.List
import Containers.NonEmpty
import Containers.Prelude
import Control.Exception
-- import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as C
import MutContainers.Map
import MutGraph.Graph
import System.IO
import Data.Char

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
      Right r -> return r
    where
      parse = do
        file <- openFile fileName ReadMode
        contents <- C.hGetContents file
        let lines = C.lines contents
        case toNonEmpty lines of
          Nothing -> return Nothing
          Just nelines -> do
            let headerLine = head nelines
            let fileLines = tail nelines
            let (nodeCount, edgeCount) =
                  case parseHeader headerLine of
                    Nothing -> (0, 0)
                    Just (a, b) -> (a, b)
            let edges = fmap _parseEdgeData fileLines
            return (Just (edges, nodeCount, edgeCount))
      parseHeader line = do
        nodeCount <- parseInt (words `at` 0)
        edgeCount <- parseInt (words `at` 1)
        return (nodeCount, edgeCount)
        where
          words = C.words line
          parseInt s = case C.readInt s of
              Nothing -> Nothing
              Just (n, _) -> Just n
      _parseEdgeData line = (t, h, w)
        where
          (t, lineAfterTail) = _parseNextInt line
          (h, lineAfterHead) = _parseNextInt lineAfterTail
          (w, _) = _parseNextInt lineAfterHead
          _parseNextInt = _parseInt . C.dropWhile isSpace
          -- non exhaustive matching is unsafe but has much less mem usage
          -- probably because of better lazy optimizations.
          _parseInt s = let Just (n, bs) = C.readInt s in (n, bs)
  {-# INLINE parseEdgesFromFileM #-}
