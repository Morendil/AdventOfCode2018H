{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day09
-- License     : BSD3

module AOC.Challenge.Day09 (
    day09a
  , day09b
  ) where

import AOC.Prelude
import Text.ParserCombinators.ReadP
import Data.List.PointedList.Circular
import qualified Data.Map as M

type GameState = (PointedList Int, Int, M.Map Int Int)

parsePair = mkPair <$> int <*> string " players; last marble is worth " <*> int <*> string " points"
  where mkPair players _ last _ = (players, last)

play :: (Int, Int) -> Int
play (players, last) = maxScore $ foldl (place players) start [1..last]

maxScore :: GameState -> Int
maxScore (_, _, scores) = snd $ maximumVal scores

start :: GameState
start = (singleton 0, 0, M.empty)

place :: Int -> GameState -> Int -> GameState
place players (balls, player, scores) ball | ball `mod` 23 == 0 = (fromJust $ deleteRight back, (player+1) `mod` players, M.alter (Just . maybe score (+score)) player scores)
  where back = moveN (-7) balls
        score = ball + _focus back
place players (balls, player, scores) ball = (insertRight ball $ next balls, (player+1) `mod` players, scores)

day09a :: (Int, Int) :~> Int
day09a = MkSol
    { sParse = parseMaybe parsePair
    , sShow  = show
    , sSolve = Just . play
    }

day09b :: (Int, Int) :~> Int
day09b = MkSol
    { sParse = parseMaybe parsePair
    , sShow  = show
    , sSolve = Just . play . (\(a,b) -> (a,b*100))
    }
