{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day20
-- License     : BSD3

module AOC.Challenge.Day20 (
    day20a
  , day20b
  , track
  , initial
  ) where

import AOC.Prelude
import Linear.V2
import qualified Data.Map as M

type Facility = M.Map (V2 Int) Int
type Exploration = (Facility, (V2 Int, [V2 Int]))

initial :: Exploration
initial = (M.empty, (V2 0 0, []))

furthest :: String -> Int
furthest = maximum . M.elems . fst . foldl (flip track) initial

realFar :: String -> Int
realFar = length . filter (>= 1000) . M.elems . fst . foldl (flip track) initial

track :: Char -> Exploration -> Exploration
track move same@(facility, (pos, stack)) = case move of
    '^' -> same
    '$' -> same
    'E' -> go (pos + (V2 0 1))
    'W' -> go (pos - (V2 0 1))
    'S' -> go (pos + (V2 1 0))
    'N' -> go (pos - (V2 1 0))
    '(' -> (facility, (pos, pos:stack))
    ')' -> (facility, (head stack, tail stack))
    '|' -> (facility, (head stack, stack))
  where sofar = fromMaybe 0 $ M.lookup pos facility
        go newPos = (M.alter incDist newPos facility, (newPos, stack))
        incDist Nothing = Just $ sofar + 1
        incDist (Just dist) = Just dist

day20a :: String :~> Int
day20a = MkSol
    { sParse = Just . head . lines
    , sShow  = show
    , sSolve = Just . furthest
    }

day20b :: _ :~> _
day20b = MkSol
    { sParse = Just . head . lines
    , sShow  = show
    , sSolve = Just . realFar
    }
