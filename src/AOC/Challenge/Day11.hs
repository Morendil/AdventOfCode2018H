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

display2 (x,y) = show x ++ "," ++ show y
display3 (x,y,z) = show x ++ "," ++ show y ++ "," ++ show z

partialSumsByRow :: Matrix Int -> Matrix Int
partialSumsByRow mat = foldl' (\m n -> combineRows n 1 (n-1) m) mat [2..(nrows mat)]

largestKSum :: [Int] -> Int -> (Int, Int)
largestKSum array k = (fst result, snd $ snd result)
  where result = foldl update starting [0..maxIdx]
        update (index, (windowSum, maxSum)) n = if newSum > maxSum
            then (n+1, (newSum, newSum))
            else (index, (newSum, maxSum))
          where newSum = windowSum - (array !! n) + (array !! (n+k))
        starting = (0, (firstSum, firstSum))
        firstSum = sum $ take k array
        maxIdx = (length array) - k - 1

allSquares :: Matrix Int -> [((Int,Int,Int),Int)]
allSquares mat = map largest allSquareCoords
  where allSquareCoords = allPairs [1..nRows mat]
        largest (y1, y2) = ((x, y1, size), s)
          where size = (y2-y1) + 1
                row = if y1 == 1 then V.toList (getRow y2) else V.toList zipWith (-) (getRow y2) (getRow (y1-1))
                (x, s) = largestKSum row size

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
    , sSolve = Just . display3 . fst . maximumBy (compare `on` snd) . allSquares . partialSumsByRow . fuelMatrix
    }
