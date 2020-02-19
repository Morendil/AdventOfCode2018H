{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day25
-- License     : BSD3

module AOC.Challenge.Day25 (
    day25a
  -- , day25b
  ) where

import AOC.Prelude

type Star = [Int]

constellations :: [Star] -> Int
constellations = length . linkConstellations

linkConstellations :: [Star] -> [[Star]]
linkConstellations (x:xs) = link [] [] [x] xs

link :: [[Star]] -> [Star] -> [Star] -> [Star] -> [[Star]]
link all [] [] [] = all
link all current [] [] = current:all
link all current [] (x:xs) = link (current:all) [] [x] xs
link all current (x:xs) stars = link all (x:current) (xs++candidates) stars'
  where linkable star = distance x star <= 3
        candidates = filter linkable stars
        stars' = filter (not.linkable) stars
        distance [x1,y1,z1,t1] [x2,y2,z2,t2] = (abs (x2-x1))+(abs (y2-y1))+(abs (z2-z1))+(abs (t2-t1))

day25a :: [Star] :~> Int
day25a = MkSol
    { sParse = Just . mapMaybe (parseMaybe intList) . lines
    , sShow  = show
    , sSolve = Just . constellations
    }

day25b :: _ :~> _
day25b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
