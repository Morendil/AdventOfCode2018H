{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day07
-- License     : BSD3

module AOC.Challenge.Day07 (
    day07a
  , day07b
  ) where

import Debug.Trace

import AOC.Prelude hiding (get)
import Text.ParserCombinators.ReadP
import qualified Data.Map as M

type Node = Char
type Edge = (Node, Node)
type Indegrees = M.Map Node Int

edge = mkEdge <$> string "Step " <*> get <*> string " must be finished before step " <*> get <*> string " can begin."
  where mkEdge _ from _ to _ = (from, to)

indegrees :: [Edge] -> Indegrees
indegrees = foldr insert M.empty
  where insert (from, to) m = M.alter (Just . maybe 1 (+1)) to $ M.alter (Just . maybe 0 id) from m

queue :: Indegrees -> [Node]
queue = M.keys . M.filter (==0)

neighbours :: [Edge] -> Node -> [Node]
neighbours edges node = map snd $ filter (\e -> fst e == node) edges

topSort :: [Edge] -> [Node]
topSort edges = go nodes (queue nodes) []
  where nodes = indegrees edges
        go :: Indegrees -> [Node] -> [Node] -> [Node]
        go nodes [] result = reverse result
        go nodes q@(x:rest) result = go decreased (sort $ queue decreased++rest) (x:result)
          where decreased = traceShowId $ foldr (M.update (Just . pred)) cleaned (traceShow x $ traceShowId $ neighbours edges x)
                cleaned = M.filter (/=0) nodes

day07a :: [Edge] :~> String
day07a = MkSol
    { sParse = parseMaybe $ sepBy1 edge (string "\n")
    , sShow  = id
    , sSolve = Just . topSort
    }

day07b :: _ :~> _
day07b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
