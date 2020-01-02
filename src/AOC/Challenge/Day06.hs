{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day06
-- License     : BSD3

module AOC.Challenge.Day06 (
    day06a
  , day06b
  ) where

import AOC.Prelude
import Text.ParserCombinators.ReadP
import Linear.V2
import Data.Ix
import Data.Tuple.HT
import qualified Data.Map as M

type Point = V2 Int
type Regions = Map Point [Point]

point = mkPoint <$> int <*> string ", " <*> int
  where mkPoint x _ y = V2 x y

distance :: Point -> Point -> Int
distance p1 p2 = sum $ abs (p2 - p1)

bounds :: [Point] -> (Point, Point)
bounds points = (V2 xMin yMin, V2 xMax yMax)
  where (Min xMin, Min yMin, Max xMax, Max yMax) = foldMap (\(V2 x y) -> (Min x, Min y, Max x, Max y)) points

addToMap :: [Point] -> Point -> Regions -> Regions
addToMap all point regions = if noTies then M.alter (Just . maybe [point] (point:)) nearest regions else regions
  where byDistance = sortOn (distance point) $ all
        noTies = (length $ head $ group $ map (distance point) byDistance) == 1
        nearest = head byDistance

makeMap :: [Point] -> Regions
makeMap points = foldr (addToMap points) M.empty $ range $ bounds points

finite :: (Point, Point) -> [Point] -> Bool
finite (min, max) = all (inRange (min+(V2 1 1),max-(V2 1 1)))

closerThan :: [Point] -> Int -> Point -> Bool
closerThan points limit point = (sum $ map (distance point) points) < limit

day06a :: [Point] :~> Int
day06a = MkSol
    { sParse = parseMaybe $ sepBy1 point (string "\n")
    , sShow  = show
    , sSolve = \points -> Just $ maximum $ map length $ filter (finite $ bounds points) $ M.elems $ makeMap points
    }

day06b :: [Point] :~> Int
day06b = MkSol
    { sParse = parseMaybe $ sepBy1 point (string "\n")
    , sShow  = show
    , sSolve = \points -> Just . length $ filter (closerThan points $ dyno_ "lim" 10000) $ range $ bounds points
    }
