{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day13
-- License     : BSD3

module AOC.Challenge.Day13 (
    day13a
  , day13b
  ) where

import AOC.Prelude
import Linear.V2
import Control.Monad.Cont
import qualified Data.Map as M

type Point = V2 Int

data Direction = North | East | South | West deriving (Eq, Show, Enum)
data Item = Fixed Char | Cart Direction

type Infra = M.Map Point Char
data MovingCart = MovingCart (V2 Int) Direction Int deriving (Eq, Show)

instance Ord MovingCart where
  compare (MovingCart (V2 x1 y1) _ _ ) (MovingCart (V2 x2 y2) _ _ ) = compare (y1,x1) (y2,x2)

type Mines = (Infra, [MovingCart])

item :: Char -> Maybe Item
item '+' = Just $ Fixed '+'
item '/' = Just $ Fixed '/'
item '\\' = Just $ Fixed '\\'
item '^' = Just $ Cart North
item 'v' = Just $ Cart South
item '>' = Just $ Cart East
item '<' = Just $ Cart West
item _ = Nothing

parse :: String -> Mines
parse input = foldMap addItem $ indexedGrid $ lines input

indexedGrid :: [String] -> [(V2 Int, Char)]
indexedGrid lines = [(V2 x y, c) | (y, line) <- indexed lines, (x, c) <- indexed line]

addItem :: (V2 Int, Char) -> (Infra, [MovingCart])
addItem i@(pos, c) = add (item c)
  where add Nothing = (mempty, [])
        add (Just (cart@(Cart dir))) = (mempty, [MovingCart pos dir 0])
        add (Just (Fixed c)) = (M.singleton pos c, [])

display2 (x,y) = show x ++ "," ++ show y

at :: V2 Int -> MovingCart -> Bool
at somePos (MovingCart pos _ _) = pos == somePos

advance :: Direction -> V2 Int -> V2 Int
advance North p = p-(V2 0 1)
advance South p = p+(V2 0 1)
advance East p = p+(V2 1 0)
advance West p = p-(V2 1 0)

left, right, curve, curve' :: Direction -> Direction
left = toEnum . (flip mod) 4 . pred . fromEnum
right = toEnum . (flip mod) 4 . succ . fromEnum
curve North = East; curve East = North; curve South = West ; curve West = South
curve' North = West; curve' West = North; curve' South = East ; curve' East = South

route :: MovingCart -> Maybe Char -> MovingCart
route cart@(MovingCart pos heading turns) c = case c of
    Just '/' -> MovingCart pos (curve heading) turns
    Just '\\' -> MovingCart pos (curve' heading) turns
    Just '+' -> case turns of
      0 -> MovingCart pos (left heading) ((turns+1) `mod` 3)
      2 -> MovingCart pos (right heading) ((turns+1) `mod` 3)
      _ -> MovingCart pos heading ((turns+1) `mod` 3)
    _ -> cart

step :: Infra -> ([MovingCart], [MovingCart]) -> Either Point ([MovingCart], [MovingCart])
step infra ([], stepped) = Right (sort stepped, [])
step infra (cart@(MovingCart before heading turns):unstepped, stepped) = if any (at pos) unstepped || any (at pos) stepped || out pos
    then Left pos
    else Right (unstepped, cart':stepped)
  where cart' = route moved terrain
        moved = MovingCart pos heading turns
        terrain = M.lookup pos infra
        pos = advance heading before
        out (V2 x y) = (x < 0 || y < 0 || x > 150 || y > 150) && error "out of bounds"

toFirstCrash :: Mines -> (Int, Int)
toFirstCrash (infra, carts) = go ([], carts)
  where go (unstepped, stepped) = case step infra (unstepped, stepped) of
          Left (V2 x y) -> (x, y)
          Right (unstepped', stepped') -> go (unstepped', stepped')

toLastCart :: Mines -> (Int, Int)
toLastCart (infra, carts) = go ([], carts)
  where go ([], [lastOne]) = pos lastOne
        go (unstepped, stepped) = case step infra (unstepped, stepped) of
          Left crash@(V2 x y) -> go (filter (not.at crash) $ tail unstepped, filter (not.at crash) stepped)
          Right (unstepped', stepped') -> go (unstepped', stepped')
        pos (MovingCart (V2 x y) _ _) = (x,y)

day13a :: Mines :~> (Int, Int)
day13a = MkSol
    { sParse = Just . parse
    , sShow  = display2
    , sSolve = Just . toFirstCrash
    }

day13b :: Mines :~> (Int, Int)
day13b = MkSol
    { sParse = Just . parse
    , sShow  = display2
    , sSolve = Just . toLastCart
    }
