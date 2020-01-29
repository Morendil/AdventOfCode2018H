{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day17
-- License     : BSD3

module AOC.Challenge.Day17 (
    day17a
  , day17b
  , parse
  , scanAll
  , doStep
  , spread
  , display
  ) where

import AOC.Prelude hiding (lefts, rights)
import Text.ParserCombinators.ReadP
import Linear.V2
import Data.List.HT hiding (range)
import qualified Data.Map as M
import qualified Data.Ix as I

spot :: ReadP (Int, Int)
spot = do
  value <- int
  return (value, value)

range  :: ReadP (Int, Int)
range = do
  vmin <- int
  string ".."
  vmax <- int
  return (vmin, vmax)

vein :: ReadP Vein
vein = xvein +++ yvein

xvein :: ReadP Vein
xvein = do
  string "x="
  (xmin, xmax) <- spot
  string ", y="
  (ymin, ymax) <- range
  return $ V2 (V2 xmin ymin) (V2 xmax ymax)

yvein :: ReadP Vein
yvein = do
  string "y="
  (ymin, ymax) <- spot
  string ", x="
  (xmin, xmax) <- range
  return $ V2 (V2 xmin ymin) (V2 xmax ymax)

type Point = V2 Int
type Vein = V2 Point
type Scan = M.Map Point Char
type Source = Point

scanVein :: Vein -> Scan
scanVein (V2 vmin vmax) = foldr (flip M.insert $ '#') M.empty (I.range (vmin, vmax))

scanAll :: [Vein] -> Scan
scanAll = foldMap scanVein

simulate :: Source -> Scan -> Scan
simulate source scan = chopY $ snd $ last $ takeUntil (null.fst) $ iterate (doStep maxY) ([source], scan)
  where (Min minY, Max maxY) = foldMap (\(V2 x y) -> (Min y, Max y)) $ M.keys scan
        chopY = M.filterWithKey (\(V2 _ y) _ -> y >= minY)

doStep :: Int -> ([Source], Scan) -> ([Source], Scan)
doStep maxY = stepSpread . stepFall maxY

stepFall :: Int -> ([Source], Scan) -> ([Source], Scan)
stepFall maxY (sources, scan) = foldl doFall ([], scan) sources
   where doFall (sources, scan) source = (nub $ sources++newSources, newScan)
           where (newSources, newScan) = fall maxY (source, scan)

stepSpread :: ([Source], Scan) -> ([Source], Scan)
stepSpread (sources, scan) = foldl doSpread ([], scan) sources
  where doSpread (sources, scan) source = (nub $ sources++newSources, newScan)
           where (newSources, newScan) = spread (source, scan)

fall :: Int -> (Source, Scan) -> ([Source], Scan)
fall maxY (source@(V2 _ y), scan) = if supported scan source then ([source], marked) else stepFall maxY (maybeFall, marked)
  where marked = flow [source] scan
        maybeFall = [source + V2 0 1 | y < maxY]

spread :: (Source, Scan) -> ([Source], Scan)
spread (source, scan) = if filling then filled else toEdges
  where lookLeft = reverse $ takeUntil (not.moving) (lefts source)
        lookRight = reverse $ takeUntil (not.moving) (rights source)
        flowing pt = (supported scan pt) && not (blocked scan pt)
        moving pt = (supported scan pt) && not (contained scan pt)
        falling pt = not (supported scan pt) && not (blocked scan pt)
        filling = (contained scan $ head lookLeft) && (contained scan $ head lookRight)
        filled = ([rise], fill (tail lookLeft++tail lookRight++[source]) scan)
        toEdges = (filter falling [head lookLeft, head lookRight], flow (tail lookLeft++tail lookRight) scan)
        rise = source - V2 0 1

fill :: [Point] -> Scan -> Scan
fill points scan = foldr (flip M.insert '~') scan points

flow :: [Point] -> Scan -> Scan
flow points scan = foldr (flip M.insert '|') scan points

at :: Scan -> Point -> Char
at scan pt = fromMaybe '.' $ M.lookup pt scan

supported :: Scan -> Point -> Bool
supported scan pt = under == '#' || under == '~'
  where under = at scan (pt + V2 0 1)

blocked :: Scan -> Point -> Bool
blocked scan pt = at scan pt /= '.'

contained :: Scan -> Point -> Bool
contained scan pt = at scan pt == '#'

lefts :: Point -> [Point]
lefts = tail . iterate (flip (-) $ V2 1 0)

rights :: Point -> [Point]
rights = tail . iterate (flip (+) $ V2 1 0)

allWater :: Scan -> Int
allWater = length . M.filter (/= '#')

standingWater :: Scan -> Int
standingWater = length . M.filter (== '~')

parse = parseMaybe $ sepBy1 vein (string "\n")

bounds :: [Point] -> (V2 Int, V2 Int)
bounds points = (V2 xMin yMin, V2 xMax yMax)
  where (Min xMin, Min yMin, Max xMax, Max yMax) = foldMap (\(V2 x y) -> (Min x, Min y, Max x, Max y)) points

display scan = [ [ rep $ M.lookup (V2 x y) scan | x <- [xMin-1..xMax+1]] | y <- [yMin-1..yMax+1]]
  where rep (Just c) = c
        rep Nothing = '.'
        (V2 xMin yMin, V2 xMax yMax) = bounds $ M.keys scan

day17a :: [Vein] :~> Int
day17a = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . allWater . simulate (V2 500 0) . scanAll
    }

day17b :: [Vein] :~> Int
day17b = MkSol
    { sParse = parse
    , sShow  = show
    , sSolve = Just . standingWater . simulate (V2 500 0) . scanAll
    }
