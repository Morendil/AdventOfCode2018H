{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3

module AOC.Challenge.Day11 (
    day11a
  , day11b
  ) where

import AOC.Prelude hiding (toList, transpose)
import Data.Function
import Linear.V2
import Data.Ix
import qualified Data.Map as M

type Point = V2 Int

fuelMatrix :: Int -> Map Point Int
fuelMatrix serial = foldr (\xy -> M.insert xy (power serial xy)) M.empty $ range ((V2 1 1),(V2 300 300))

power :: Int -> Point -> Int
power serial (V2 x y) = hundreds - 5
  where hundreds = (powerLevel `div` 100) `mod` 10
        powerLevel = ((rackId * y) + serial) * rackId
        rackId = x + 10

allLevels :: Map Point Int -> [((Int,Int),Int)]
allLevels mat = [((x,y), rangeSum mat (V2 x y, V2 (x+2) (y+2)))| x <- [1..298], y <- [1..298]]
  where rangeSum mat r = sum $ catMaybes $ map (flip M.lookup mat) $ range r

display2 (x,y) = show x ++ "," ++ show y
display3 (x,y,z) = show x ++ "," ++ show y ++ "," ++ show z

summedAreaTable :: Map Point Int -> Map Point Int
summedAreaTable mat = force sat
  where sat = M.mapWithKey rollUp mat
        rollUp (V2 x y) val = sum $ catMaybes [Just val, above, left, fmap negate diagonal]
          where above = M.lookup (V2 x (y-1)) sat
                left = M.lookup (V2 (x-1) y) sat
                diagonal = M.lookup (V2 (x-1) (y-1)) sat

allSquares :: Map Point Int -> [((Int,Int,Int),Int)]
allSquares mat = [((x,y,side), sumAt (x,y,side)) | side <-[1..300], x <- [1..(300 - side - 1)], y <- [1..(300 - side - 1)]]
  where sumAt (x,y,1) = fromMaybe 0 $ M.lookup (V2 x y) mat
        sumAt (x,y,side) = topLeft+bottomRight-bottomLeft-topRight
          where bottomRight = fromMaybe 0 $ M.lookup (V2 (x+side-1) (y+side-1)) mat
                topLeft = fromMaybe 0 $ M.lookup (V2 (x-1) (y-1)) mat
                bottomLeft = fromMaybe 0 $ M.lookup (V2 (x-1) (y+side-1)) mat
                topRight = fromMaybe 0 $ M.lookup (V2 (x+side-1)( y-1)) mat

day11a :: Int :~> String
day11a = MkSol
    { sParse = Just . read
    , sShow  = id
    , sSolve = Just . display2 . fst . maximumBy (compare `on` snd) . allLevels . fuelMatrix
    }

day11b :: Int :~> String
day11b = MkSol
    { sParse = Just . read
    , sShow  = id
    , sSolve = Just . display3 . fst . maximumBy (compare `on` snd) . allSquares . summedAreaTable . fuelMatrix
    }
