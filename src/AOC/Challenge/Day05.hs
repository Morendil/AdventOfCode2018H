{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3

module AOC.Challenge.Day05 (
    day05a
  , day05b
  , react
  , reactOne
  ) where

import AOC.Prelude
import Data.List.HT
import Debug.Trace

react :: String -> String
react = snd . last . takeUntil (\(a,b) -> length a == length b) . oneAndNext . iterate reactOne

reactOne :: String -> String
reactOne [] = []
reactOne [x] = [x]
reactOne (x:y:rest) | toLower x == toLower y && x /= y = (reactOne rest)
reactOne (x:rest) = x:(reactOne rest)

day05a :: String :~> Int
day05a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . length . react
    }

day05b :: String :~> Int
day05b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . length
    }
