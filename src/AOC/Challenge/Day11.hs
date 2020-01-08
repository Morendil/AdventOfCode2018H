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
import Data.Matrix
import Data.Function
import qualified Data.Vector as V

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

summedAreaTable :: Matrix Int -> Matrix Int
summedAreaTable = transpose . partialSumsByRow . transpose . partialSumsByRow

allSquares :: Matrix Int -> [((Int,Int,Int),Int)]
allSquares mat = [((x,y,side), sumAt (x,y,side)) | side <-[1..nrows mat], x <- [1..(300 - side - 1)], y <- [1..(300 - side - 1)]]
  where sumAt (x,y,1) = getElem y x mat
        sumAt (x,y,side) = topLeft+bottomRight-bottomLeft-topRight
          where bottomRight = getElem (y+side-1) (x+side-1) mat
                topLeft = if x>1 && y > 1 then getElem (y-1) (x-1) mat else 0
                bottomLeft = if x > 1 then getElem (y+side-1) (x-1) mat else 0
                topRight = if y > 1 then getElem (y-1) (x+side-1) mat else 0

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
