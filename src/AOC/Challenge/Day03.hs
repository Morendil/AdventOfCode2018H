{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3

module AOC.Challenge.Day03 (
    day03a
  , day03b
  , Claim(..)
  , overlap
  ) where

import           AOC.Prelude
import Text.ParserCombinators.ReadP
import qualified Data.Set as Set
import qualified Data.Map as Map

type Layers = Map Position Int
type Position = (Int, Int)
type Extent = (Int, Int)
data Claim = Claim { claimId :: Int, pos :: Position, ext :: Extent} deriving (Eq, Show)

parseClaim =
  mkClaim <$> string "#" <*> int <*> string " @ " <*> int <*> comma <*> int <*> string ": " <*> int <*> string "x" <*> int
  where mkClaim _ id _ px _ py _ w _ h = Claim id (px, py) (w, h)

squares :: Claim -> [Position]
squares (Claim {pos=(px,py), ext=(w,h)}) = [(x,y) | x <-[px..(px+w)-1], y <-[py..(py+h)-1]]

insertClaim :: Claim -> Layers -> Layers
insertClaim claim map = foldr insertSquare map (squares claim)
  where insertSquare square map = Map.alter (Just . maybe 1 (+1)) square map

layers :: [Claim] -> Layers
layers = foldr insertClaim Map.empty

overlap :: Claim -> Claim -> [Position]
overlap (Claim id1 (x1,y1) (w1,h1)) (Claim id2 (x2,y2) (w2,h2)) = [(x,y) | 
  x <- [(max x1 x2)..(min (x1+w1) (x2+w2))-1],
  y <- [(max y1 y2)..(min (y1+h1) (y2+h2))-1]
  ]

day03a :: [Claim] :~> Int
day03a = MkSol
    { sParse = parseMaybe (sepBy1 parseClaim (string "\n"))
    , sShow  = show
    , sSolve = Just . length . Map.filter (>1) . layers
    }

day03b :: [Claim] :~> Int
day03b = MkSol
    { sParse = parseMaybe (sepBy1 parseClaim (string "\n"))
    , sShow  = show
    , sSolve = Just . claimId . head . map (fst.fromJust) . concat . filter (all isJust) . crossWith (\a b -> (a,b) <$ guard (null $ overlap a b))
    }
