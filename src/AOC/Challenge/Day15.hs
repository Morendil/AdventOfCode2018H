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
  , doRound
  , parse
  , Fight(..)
  , outcome
  ) where

import AOC.Prelude
import Linear.V2
import Algorithm.Search
import Data.List.HT
import Data.List.Extra
import qualified Data.Map as M
import qualified Data.Set as S

newtype Point = Pt (V2 Int) deriving (Eq, Show)
instance Ord Point where
  compare (Pt (V2 x1 y1)) (Pt (V2 x2 y2)) = compare (y1,x1) (y2,x2)

data Unit = Floor | Unit Char Int Int deriving (Eq, Show)
data Fight = Done Caves Int | Fighting Caves Int deriving (Eq, Show)

type Caves = M.Map Point Unit
type Path = [Point]

parse :: String -> Caves
parse input = foldMap addItem $ indexedGrid $ lines input

indexedGrid :: [String] -> [(Point, Char)]
indexedGrid lines = [(Pt (V2 x y), c) | (y, line) <- indexed lines, (x, c) <- indexed line]

item :: Char -> Maybe Unit
item '.' = Just $ Floor
item 'G' = Just $ Unit 'G' 3 200
item 'E' = Just $ Unit 'E' 3 200
item _ = Nothing

addItem :: (Point, Char) -> Caves
addItem (pos, c) = add (item c)
  where add Nothing = mempty
        add (Just unit) = (M.singleton pos unit)

floors :: Caves -> [Point]
floors = M.keys . M.filter isFloor
  where isFloor Floor = True
        isFloor _ = False

armies :: Caves -> [Point]
armies = M.keys . M.filter isMobile
  where isMobile Floor = False
        isMobile _ = True

hitpoints :: Unit -> Int
hitpoints Floor = 0
hitpoints (Unit _ _ hp) = hp

near :: Point -> [Point]
near (Pt v) = map (\o -> Pt (v+o)) [V2 1 0, V2 0 1, V2 (-1) 0, V2 0 (-1)]

neighbours :: Caves -> Point -> [Point]
neighbours caves pt = intersect (floors caves) (near pt)

targets :: Char -> Caves -> [Point]
targets us = M.keys . M.filter isOpponent
  where isOpponent (Unit them _ _) = us /= them
        isOpponent _ = False

inRange :: Caves -> [Point] -> [Point]
inRange caves = foldMap (neighbours caves)

path :: Caves -> Point -> Point -> Maybe [Point]
path caves from to = fmap snd $ dijkstra (reverse . sort . neighbours caves) (\_ _ -> 1) (== to) from

shortest :: [Path] -> [Path]
shortest paths = filter (\p -> length p == minLen) paths
  where minLen = minimum $ map length paths

chosen :: [Path] -> [Path]
chosen paths = filter (\p -> last p == nearest) paths
  where nearest = minimum $ map last paths

step :: [Path] -> Point
step = minimum . map head

doTurn :: Fight -> Point -> Fight
doTurn f@(Done _ _) spot = f
doTurn (Fighting caves rounds) spot = if dead || null opponents then (Done caves rounds) else Fighting (turn caves) rounds
  where turn = maybeMove . maybeAttack
        maybeMove = if null movements || (not.null) (attackable spot) then id else moveTo newSpot
        maybeAttack = if null (attackable newSpot) then id else M.update dealDamage (attacked newSpot)
        dealDamage (Unit f_ a_ hp) = if (hp-atk) > 0 then Just $ Unit f_ a_ (hp-atk) else Just Floor
        dealDamage _ = error "Attacking the floor ?"
        attacked = minimumOn (\dest -> (hitpoints $ fromJust $ M.lookup dest caves, dest)) . attackable
        attackable dest = intersect opponents $ near dest
        moveTo dest = M.insert dest unit . M.insert spot Floor
        newSpot = if null movements || (not.null) (attackable spot) then spot else (step . chosen . shortest) movements
        movements = mapMaybe (path caves spot) $ inRange caves opponents
        opponents = targets faction caves
        me = M.lookup spot caves
        dead = me == Just Floor
        unit@(Unit faction atk _) = fromJust me

doRound :: Fight -> Fight
doRound f@(Done _ _) = f
doRound f@(Fighting caves rounds) = incRound $ foldl doTurn f $ armies caves
  where incRound (Fighting caves rounds) = Fighting caves (rounds+1)
        incRound f = f

outcome :: Fight -> Int
outcome fight = rounds * totalHitpoints
  where (units, rounds) = case runFight fight of (Done caves _rounds) -> (caves, _rounds); (Fighting caves _rounds) -> (caves, _rounds)
        runFight = last . takeUntil done . iterate doRound
        totalHitpoints = sum $ map hitpoints $ M.elems units
        done (Done _ _ ) = True
        done _ = False

initial :: Caves -> Fight
initial caves = Fighting caves 0

day15a :: Caves :~> Int
day15a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . outcome . initial
    }

day15b :: Caves :~> Int
day15b = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = const $ Just 0
    }
