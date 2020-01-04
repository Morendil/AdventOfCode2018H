{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day10
-- License     : BSD3

module AOC.Challenge.Day10 (
    day10a
  , day10b
  ) where

import AOC.Prelude
import Text.ParserCombinators.ReadP
import Linear.V2
import Data.Ix

import Debug.Trace

type Star = V2 (V2 Int)
type Point = V2 Int

star = mkStar <$> string "position=<" <*> int' <*> comma <*> int' <*> string "> velocity=<" <*> int' <*> comma <*> int' <*> string ">"
  where mkStar _ x _ y _ vx _ vy _ = V2 (V2 x y) (V2 vx vy)

evolve :: Star -> Star
evolve (V2 pos vel) = (V2 (pos+vel) vel)

surface :: [Star] -> Int
surface stars = (xMax-xMin) * (yMax-yMin)
  where positions = map pos stars
        pos (V2 p _) = p
        (Min xMin, Min yMin, Max xMax, Max yMax) = foldMap (\(V2 x y) -> (Min x, Min y, Max x, Max y)) positions

bounds :: [Point] -> (Point, Point)
bounds points = (V2 xMin yMin, V2 xMax yMax)
  where (Min xMin, Min yMin, Max xMax, Max yMax) = foldMap (\(V2 x y) -> (Min x, Min y, Max x, Max y)) points

display :: [Star] -> String
display stars = unlines $ ["..."] ++ [[if elem (V2 x y) positions then '#' else '.' | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where (V2 xMin yMin, V2 xMax yMax) = bounds positions
        positions = map pos stars
        pos (V2 p _) = p

day10a :: [Star] :~> String
day10a = MkSol
    { sParse = parseMaybe $ sepBy1 star (string "\n")
    , sShow  = id
    , sSolve = Just . display . snd . last . takeWhile (\(a,b) -> surface b < surface a) . oneAndNext . iterate (map evolve)
    }

day10b ::  [Star] :~> Int
day10b = MkSol
    { sParse = \input -> parseMaybe (sepBy1 star (string "\n")) input
    , sShow  = show
    , sSolve = Just . length . takeWhile (\(a,b) -> surface b < surface a) . oneAndNext . iterate (map evolve)
    }
