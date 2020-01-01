{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3

module AOC.Challenge.Day02 (
    day02a
  , day02b
  ) where

import           AOC.Prelude
import Data.List
import qualified Data.Text as T
import Data.Text.Metrics

filterByCount :: [String] -> Int -> [String]
filterByCount boxes n = filter (\id -> any (==n) $ map length $ group $ sort id) boxes

day02a :: [String] :~> Int
day02a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = \boxes -> Just $ product $ map length $ map (filterByCount boxes) [2,3]
    }

day02b :: [String] :~> String
day02b = MkSol
    { sParse = Just . lines
    , sShow  = id
    , sSolve = \ids -> listToMaybe $ [intersect id1 id2 | id1 <- ids, id2 <- ids, (hamming (T.pack id1) (T.pack id2)) == Just 1]
    }
