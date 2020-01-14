{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day15
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable

module AOC.Challenge.Day15 (
    day15a
  , day15b
  ) where

import AOC.Prelude
import Linear.V2
import Algorithm.Search
import qualified Data.Map as M
import qualified Data.Set as S

day15a :: Caves :~> Int
day15a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = const $ Just 0
    }

day15b :: Caves :~> Int
day15b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = const $ Just 0
    }
