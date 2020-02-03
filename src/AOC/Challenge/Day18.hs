{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day18
-- License     : BSD3

module AOC.Challenge.Day18 (
    day18a
  , day18b
  ) where

import AOC.Prelude
import Linear.V2
import qualified Data.Map as M

type Point = V2 Int
type Scan = M.Map Point Char

indexedGrid :: [String] -> [(V2 Int, Char)]
indexedGrid gridLines = [(V2 x y, c) | (y, line) <- indexed gridLines, (x, c) <- indexed line]

resourceValue :: Scan -> Int
resourceValue scan = lumber * trees
  where [lumber, trees] = map (\r -> countElem r $ M.elems scan) ['#','|']
        countElem i = length . filter (i==)

bounds :: [Point] -> (V2 Int, V2 Int)
bounds points = (V2 xMin yMin, V2 xMax yMax)
  where (Min xMin, Min yMin, Max xMax, Max yMax) = foldMap (\(V2 x y) -> (Min x, Min y, Max x, Max y)) points

display :: Scan -> [String]
display scan = [ [ fromJust $ M.lookup (V2 x y) scan | x <- [xMin..xMax]] | y <- [yMin..yMax]]
  where (V2 xMin yMin, V2 xMax yMax) = bounds $ M.keys scan

evolve :: Scan -> Scan
evolve scan = M.mapWithKey (evolveCell scan) scan

offsets :: [Point]
offsets = [V2 (-1) (-1), V2 0 (-1), V2 1 (-1), V2 (-1) 0, V2 1 0, V2 (-1) 1, V2 0 1, V2 1 1]

neighbours :: Scan -> Point -> [Char]
neighbours scan point = mapMaybe ((flip M.lookup $ scan) . (+ point)) offsets

evolveCell :: Scan -> Point -> Char -> Char
evolveCell scan pos prev = case prev of
    '.' -> if trees >= 3 then '|' else '.'
    '|' -> if lumber >= 3 then '#' else '|'
    '#' -> if lumber >= 1  && trees >= 1 then '#' else '.'
    _ -> error "Uh?"
  where [lumber, trees] = map (\r -> countElem r $ neighbours scan pos) ['#','|']
        countElem i = length . filter (i==)

resultAfter :: Int -> Scan -> Scan
resultAfter n initial = iterate evolve initial !! n

resultAfterMany :: Int -> Scan -> Scan
resultAfterMany n initial = finalState
  where states = iterate evolve initial
        converged = fromJust $ firstRepeated $ states
        [one, two] = take 2 $ elemIndices converged states
        period = two - one
        finalState = states !! (one + snd ((n+1) `divMod` (two-one)))

day18a :: Scan :~> Int
day18a = MkSol
    { sParse = Just . M.fromList . indexedGrid . lines
    , sShow  = show
    , sSolve = Just . resourceValue . resultAfter 10
    }

day18b :: Scan :~> Int
day18b = MkSol
    { sParse = Just . M.fromList . indexedGrid . lines
    , sShow  = show
    , sSolve = Just . resourceValue . resultAfterMany 1000000000
    }
