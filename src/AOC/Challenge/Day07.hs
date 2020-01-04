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
import Data.List.HT
import qualified Data.Map as M

type Node = Char
type Edge = (Node, Node)
type Indegrees = M.Map Node Int
type Cost = Node -> Int

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
          where decreased = foldr (M.update (Just . pred)) cleaned (neighbours edges x)
                cleaned = M.filter (/=0) nodes

type SortState = (Indegrees, [Node], Int, M.Map Node Int)

topSortParallel :: Cost -> Int -> [Edge] -> Int
topSortParallel cost workers edges = lastTime $ last $ takeUntil queueEmpty $ iterate (sortStep cost workers edges) (initialState cost workers (nodes, queue nodes, 0, M.empty))
  where nodes = indegrees edges
        lastTime :: SortState -> Int
        lastTime (_, _, time, _) = time
        queueEmpty :: SortState -> Bool
        queueEmpty (_,q,_,_) = null q

initialState :: Cost -> Int -> SortState -> SortState
initialState cost workers s@(nodes, queue, time, assigned) = (nodes, queue, time, assigned')
  where assigned' = assign cost workers s

assign :: Cost -> Int -> SortState -> M.Map Node Int
assign cost workers (nodes, queue, time, assigned) = foldr (\n -> M.insert n (time+cost n)) assigned $ take max $ unassigned
  where max = workers - M.size assigned
        unassigned = queue \\ M.keys assigned

sortStep :: Cost -> Int -> [Edge] -> (Indegrees, [Node], Int, M.Map Node Int) -> (Indegrees, [Node], Int, M.Map Node Int)
sortStep cost workers edges (nodes, [], time, assignments) = (nodes, [], time, assignments)
sortStep cost workers edges (nodes, q, time, assignments) = (decreased, queued, time', assignments')
  where dequeued = fst $ minimumVal assignments
        ongoing = M.delete dequeued assignments
        queued = sort $ (queue decreased)++(delete dequeued q)
        decreased = foldr (M.update (Just . pred)) cleaned (neighbours edges dequeued)
        cleaned = M.filter (/=0) nodes
        assignments' = assign cost workers (nodes, queued, time', ongoing)
        time' = snd $ minimumVal assignments

day07a :: [Edge] :~> String
day07a = MkSol
    { sParse = parseMaybe $ sepBy1 edge (string "\n")
    , sShow  = id
    , sSolve = Just . topSort
    }

day07b :: [Edge] :~> Int
day07b = MkSol
    { sParse = parseMaybe $ sepBy1 edge (string "\n")
    , sShow  = show
    , sSolve = Just . topSortParallel (\c -> (ord c) - 64 + (dyno_ "wait" 60)) (dyno_ "cap" 5)
    }
