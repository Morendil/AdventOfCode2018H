{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import AOC.Prelude
import qualified Data.Set as Set

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = Just . map (read.filter (/= '+')) . lines
    , sShow  = show
    , sSolve = Just . sum
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = Just . map (read.filter (/= '+')) . lines
    , sShow  = show
    , sSolve = firstRepeated . scanl (+) 0 . cycle
    }
