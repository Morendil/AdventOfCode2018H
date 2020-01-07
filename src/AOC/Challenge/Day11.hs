{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day11
-- License     : BSD3

module AOC.Challenge.Day11 (
    day11a
  , day11b
  , indexOfLargestKSum
  ) where

import AOC.Prelude hiding (toList)
import Data.Matrix
import Data.Function

fuelMatrix :: Int -> Matrix Int
fuelMatrix serial = matrix 300 300 (power serial)

power :: Int -> (Int, Int) -> Int
power serial (y,x) = hundreds - 5
  where hundreds = (powerLevel `div` 100) `mod` 10
        powerLevel = ((rackId * y) + serial) * rackId
        rackId = x + 10

allLevels :: Matrix Int -> [((Int,Int),Int)]
allLevels mat = [((x,y), sum $ toList $ submatrix y (y+2) x (x+2) mat)| x <- [1..298], y <- [1..298]]

display (x,y) = show x ++ "," ++ show y

partialSumsByRow :: Matrix Int -> Matrix Int
partialSumsByRow mat = foldl' (\m n -> combineRows n 1 (n-1) m) mat [2..(nrows mat)]

indexOfLargestKSum :: [Int] -> Int -> Int
indexOfLargestKSum array k = undefined

day11a :: Int :~> String
day11a = MkSol
    { sParse = Just . read
    , sShow  = id
    , sSolve = Just . display . fst . maximumBy (compare `on` snd) . allLevels . fuelMatrix
    }

day11b :: Int :~> String
day11b = MkSol
    { sParse = Just . read
    , sShow  = id
    , sSolve = Just . show
    }
