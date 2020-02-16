{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day22
-- License     : BSD3

module AOC.Challenge.Day22 (
    day22a
  , day22b
  ) where

import AOC.Prelude
import Text.ParserCombinators.ReadP
import Data.HashSet hiding (map, filter, foldr)
import Data.Hashable
import Data.List
import Data.Maybe
import Data.MemoTrie

import qualified Data.HashPSQ as PSQ
import qualified Data.Map as M

type Mission = (Int, (Int, Int))

mission :: ReadP Mission
mission = mkMission <$> string "depth: " <*> int <*> string "\ntarget: " <*> int <*> string "," <*> int
  where mkMission _ depth _ tx _ ty = (depth, (tx, ty))

index :: Int -> (Int, Int) -> (Int, Int) -> Int
index _ _ (0, 0) = 0
index _ _ (x, 0) = x * 16807
index _ _ (0, y) = y * 48271
index depth target coords | target == coords = 0
index depth target (x, y) = (erosionmem depth target (x-1,y)) * (erosionmem depth target (x,y-1))

erosion :: Int -> (Int, Int) -> (Int, Int) -> Int
erosion depth target coords = ((indexmem depth target coords) + depth) `mod` 20183

terrain :: Int -> (Int, Int) -> (Int, Int) -> Int
terrain depth target coords = (erosionmem depth target coords) `mod` 3

indexmem = memo3 index
erosionmem = memo3 erosion
terrainmem = memo3 terrain

riskLevel :: Mission -> Int
riskLevel (depth, target@(tx, ty))= sum [terrain depth target (x,y) | x <- [0..tx], y <- [0..ty]]

landscape :: Int -> (Int, Int) -> (Int, Int) -> [[Char]]
landscape depth target (tx, ty) = [[t (terrain depth target (x, y)) | x <- [0..tx]] | y <- [0..ty]]
  where t 0 = '.'
        t 1 = '='
        t 2 = '|'

moveCost :: (Int, Int, Int) -> (Int, Int, Int) -> Int
moveCost (fx, fy, ft) (tx, ty, tt) = if ft == tt then 1 else 7

offsets :: [(Int, Int, Int)]
offsets = [(-1,0,0),(0,1,0),(0,-1,0),(1,0,0),(0,0,-1),(0,0,1),(0,0,-2),(0,0,2)]

okFor :: Int -> Int -> Bool
okFor tool 0 = tool == 0 || tool == 1
okFor tool 1 = tool == -1 || tool == 1
okFor tool 2 = tool == -1 || tool == 0

add (x1, y1, t1) (x2, y2, t2) = (x1+x2, y1+y2, t1+t2)

data AStarState a c = AStarState {
  closedList :: [a],
  openList :: PSQ.HashPSQ a c (),
  costs :: M.Map a c,
  back :: M.Map a a,
  found :: Maybe a
}

initAStar start = AStarState {
  closedList = [start],
  openList = PSQ.empty,
  costs = M.empty,
  back = M.empty,
  found = Nothing
}

doAStar :: (Foldable f, Num cost, Ord cost, Ord state, Hashable state)
  => (state -> f state)
  -> (state -> state -> cost)
  -> (state -> cost)
  -> (state -> Bool)
  -> AStarState state cost
  -> Maybe (AStarState state cost)
doAStar neighbours cost heuristic goal state = case PSQ.minView $ openList state of
  Nothing -> Nothing
  Just (x, _, _, _) | goal x -> Just $ state { found = Just x }
  Just (x, _, _, xs) -> Just $ state' { closedList = x:(closedList state'), openList = xs }
    where state' = foldr maybeInsert state (neighbours x)
          maybeInsert candidate searchState = let itsCost = cost x candidate in
            if elem candidate (closedList searchState) || maybe False (< itsCost) (M.lookup candidate (costs searchState))
            then searchState
            else searchState {
              costs = M.insert candidate itsCost (costs searchState),
              back = M.insert candidate x (back searchState),
              openList = PSQ.insert candidate itsCost () (openList searchState)
              }

aStar :: (Foldable f, Num cost, Ord cost, Ord state, Hashable state)
  => (state -> f state)
  -> (state -> state -> cost)
  -> (state -> cost)
  -> (state -> Bool)
  -> state
  -> Maybe (cost, [state])
aStar neighbours cost heuristic goal initial = getCostAndPath <$> endState
  where endState = doAStar neighbours cost heuristic goal (initAStar initial)
        getCostAndPath s = let path = getPath s in (sum $ map ((costs s) M.!) $ path, path)
        getPath s = reverse $ takeWhile (/= initial) $ iterate ((back s) M.!) (fromJust $ found s)

path :: Int -> (Int, Int) -> Maybe (Int, [(Int, Int, Int)])
path depth target@(tx, ty) = aStar (neighboursmem depth target) moveCost heuristic goal start
  where start = (0, 0, 0)
        goal pos = pos == (tx, ty, 0)
        heuristic (x,y,_) = x+y

neighboursmem = memo3 neighbours

neighbours :: Int -> (Int, Int) -> (Int, Int, Int) -> HashSet (Int, Int, Int)
neighbours depth target cell = fromList $ filter allowed $ map (add cell) offsets
  where allowed (a,b,c)= a >= 0 && b >= 0 && c >= -1 && c <= 1 && suitable (a,b) c
        suitable coord tool = okFor tool (terrainmem depth target coord)

pathCost :: Mission -> Int
pathCost (depth, target) = fst $ fromJust $ path depth target

day22a :: Mission :~> Int
day22a = MkSol
    { sParse = parseMaybe mission
    , sShow  = show
    , sSolve = Just . riskLevel
    }

day22b :: Mission :~> Int
day22b = MkSol
    { sParse = parseMaybe mission
    , sShow  = show
    , sSolve = Just . pathCost
    }
