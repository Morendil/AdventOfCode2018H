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

type Cell = (Int, Int, Int)

data AStarState = AStarState {
  closedList :: [Cell],
  openList :: PSQ.HashPSQ Cell Int (),
  costs :: M.Map Cell Int,
  back :: M.Map Cell Cell,
  found :: Maybe Cell
} deriving (Eq, Show)

initAStar start = AStarState {
  closedList = [],
  openList = PSQ.singleton start 0 (),
  costs = M.singleton start 0,
  back = M.empty,
  found = Nothing
}

yy (x, y, z) = y
xx (x, y, z) = x

debug :: AStarState -> String
debug aStarState = unlines {-- $ [[disp (x,y) | x <- [0..maxx]] | y <- [0..maxy]] ++ --} [show maxx++","++show maxy]
  where cells = M.keys $ costs aStarState
        maxy = maximum $ map yy cells
        maxx = maximum $ map xx cells
        disp (x,y) = if (x,y,0) `elem` cells then 'N' else if (x,y,-1) `elem` cells then 'T' else if (x,y,1) `elem` cells then 'G' else '.'

-- show (length $ closedList aStarState) ++ ", " ++ show (maximum $ map yy $ M.keys $ costs aStarState) ++ "," ++ show (maximum $ map xx $ M.keys $ costs aStarState) ++ ", " ++ show (maximum $ M.elems $ costs aStarState)

doAStar :: (Foldable f)
  => (Cell -> f Cell)
  -> (Cell -> Cell -> Int)
  -> (Cell -> Int)
  -> (Cell -> Bool)
  -> AStarState
  -> Maybe AStarState
doAStar neighbours cost heuristic goal aStarState = trace (debug aStarState) $ case PSQ.minView $ openList aStarState of
  Nothing -> Nothing
  Just (x, _, _, _) | goal x -> Just $ aStarState { found = Just x }
  Just (x, _, _, xs) -> doAStar neighbours cost heuristic goal $ aStarState' { closedList = x:(closedList aStarState') }
    where aStarState' = foldr maybeInsert (aStarState { openList = xs }) (neighbours x)
          maybeInsert candidate searchState = let baseCost = (costs searchState) M.! x
                                                  itsCost = cost x candidate + baseCost in
            if elem candidate (closedList searchState) || maybe False (< itsCost) (M.lookup candidate (costs searchState))
            then searchState
            else searchState {
              costs = M.insert candidate itsCost (costs searchState),
              back = M.insert candidate x (back searchState),
              openList = PSQ.insert candidate (baseCost + heuristic candidate) () (openList searchState)
              }

aStar :: (Foldable f)
  => (Cell -> f Cell)
  -> (Cell -> Cell -> Int)
  -> (Cell -> Int)
  -> (Cell -> Bool)
  -> Cell
  -> Maybe (Int, [Cell])
aStar neighbours cost heuristic goal initial = getCostAndPath <$> endState
  where endState = doAStar neighbours cost heuristic goal (initAStar initial)
        getCostAndPath s = let path = getPath s in ((costs s) M.! (last path), path)
        getPath s = reverse $ takeWhile (/= initial) $ iterate ((back s) M.!) (fromJust $ found s)

path :: Int -> (Int, Int) -> Maybe (Int, [(Int, Int, Int)])
path depth target@(tx, ty) = aStar (neighboursmem depth target) moveCost heuristic goal start
  where start = (0, 0, 0)
        goal pos = pos == (tx, ty, 0)
        heuristic (x,y,z) = abs (ty-y) + abs (tx-x) + abs (z*7)

neighboursmem = memo3 neighbours

neighbours :: Int -> (Int, Int) -> (Int, Int, Int) -> HashSet (Int, Int, Int)
neighbours depth target cell = fromList $ filter allowed $ map (add cell) offsets
  where allowed (a,b,c)= a >= 0 && b >= 0 && c >= -1 && c <= 1 && suitable (a,b) c
        suitable coord tool = okFor tool (terrainmem depth target coord)

pathCost :: Mission -> Int
pathCost (depth, target) = fst $ fromJust $ traceShowId $ path depth target

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
