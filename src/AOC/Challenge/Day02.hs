{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!
--
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day02 (
    day02a
  -- , day02b
  ) where

import           AOC.Prelude

filterByCount :: [String] -> Int -> [String]
filterByCount boxes n = filter (\id -> any (==n) $ map length $ group $ sort id) boxes

day02a :: [String] :~> Int
day02a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = \boxes -> Just $ product $ map length $ map (filterByCount boxes) [2,3]
    }

day02b :: _ :~> _
day02b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
