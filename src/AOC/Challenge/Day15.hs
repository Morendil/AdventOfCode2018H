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
import Data.List.Extra
import Data.List.HT
import Algorithm.Search
import qualified Data.Map as M
import qualified Data.Set as S

newtype Point = Pt (V2 Int) deriving (Eq, Show)
instance Ord Point
  where compare (Pt (V2 x1 y1)) (Pt (V2 x2 y2)) = compare (y1, x1) (y2, x2)

data Unit = Unit Char Int Int deriving (Eq, Show)
type Terrain = S.Set Point
type Units = M.Map Point Unit

type Caves = (Terrain, Units)
data Fight = Done Caves Int | Fighting Caves Int

doTurn :: Fight -> Point -> Unit -> Fight
doTurn done@(Done _ _) _ _ = done
doTurn (Fighting caves@(terrain, units) rounds) spot me@(Unit us pow _) = if fightOver then done else Fighting (terrain, turn units) rounds
  where turn = if null paths then id else maybeMove . maybeAttack
        targetPath = minimumOn length paths
        maybeMove = if length targetPath > 1 then (M.insert (head targetPath) me) . (M.delete spot) else id
        maybeAttack = if length targetPath <= 2 then M.update (Just . damage) (last targetPath) else id
        damage (Unit faction attack hp) = Unit faction attack (hp-pow)
        paths = mapMaybe (path terrain spot) $ M.keys targets
        targets = M.filter (\(Unit faction _ _) -> faction /= us) units
        fightOver = null targets
        done = Done caves rounds

offsets :: [V2 Int]
offsets = [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]

path :: Terrain -> Point -> Point -> Maybe [Point]
path terrain start (Pt target) = fmap snd $ aStar neighbours cost heuristic isGoal start
  where neighbours (Pt from) = S.intersection terrain $ S.fromList $ map (\x -> Pt (from+x)) offsets
        cost :: Point -> Point -> Int
        cost = const $ const 1
        heuristic :: Point -> Int
        heuristic (Pt from) = sum $ abs $ target-from
        isGoal = (== Pt target)

doRound :: Fight -> Fight
doRound done@(Done caves rounds) = done
doRound (Fighting caves rounds) = case undefined of
  f@(Done _ _) -> f
  f@(Fighting (terrain, units) rounds) -> (Fighting (terrain, units') (rounds+1))
    where (Fighting (_, units') _) = M.foldlWithKey doTurn f units

addItem :: (Point, Char) -> Caves
addItem (pos, what) = case what of
  '.' -> (S.singleton pos, mempty)
  'G' -> (S.singleton pos, M.singleton pos $ Unit 'G' 3 200)
  'E' -> (S.singleton pos, M.singleton pos $ Unit 'E' 3 200)
  _ -> (mempty, mempty)

indexedGrid :: [String] -> [(Point, Char)]
indexedGrid lines = [(Pt (V2 x y), c) | (y, line) <- indexed lines, (x, c) <- indexed line]

parse :: String -> Caves
parse input = foldMap addItem $ indexedGrid $ lines input

outcome :: Fight -> Int
outcome (Done (_, units) rounds) = rounds * totalHp
  where totalHp = sum $ map hitpoints $ M.elems units
        hitpoints (Unit _ _ hp) = hp
outcome (Fighting _ _) = undefined

done :: Fight -> Bool
done (Done _ _) = True
done _ = False

day15a :: Caves :~> Int
day15a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = \caves -> Just $ outcome $ last $ takeUntil done $ iterate doRound (Fighting caves 0)
    }

day15b :: Caves :~> Int
day15b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = const $ Just 0
    }
