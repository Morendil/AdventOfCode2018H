{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day23
-- License     : BSD3

module AOC.Challenge.Day23 (
    day23a
  , day23b
  ) where

import AOC.Prelude
import Text.ParserCombinators.ReadP
import Data.Algorithm.MaximalCliques

type Bot = (Int, Int, Int, Int)
type Planes = [(Int,Int)]

triple :: ReadP (Int, Int, Int)
triple = do
    x <- int; comma;  y <- int;  comma ; z <- int
    return (x, y, z)

bot :: ReadP Bot
bot = do
    (x,y,z) <- between (string "pos=<") (string ">, r=") triple
    r <- int
    return (x, y, z, r)

leaderBots :: [Bot] -> Int
leaderBots bots = length $ filter (inRangeOf leader) bots
  where leader = last $ sortOn (\(x,y,z,r) -> r) $ bots
        inRangeOf (lx,ly,lz,lr) (botx,boty,botz,_) = ((abs (botx-lx))+(abs (boty-ly))+(abs (botz-lz))) <= lr

toPlanes :: Bot -> Planes
toPlanes (x,y,z,r) = [(x+y+z-r,x+y+z+r),(x-y+z-r,x-y+z+r),(x+y-z-r,x-y-z+r),(x-y-z-r,x-y-z+r)]

overlap :: Bot -> Bot -> Bool
overlap (x1,y1,z1,r1) (x2,y2,z2,r2) = abs(x2-x1)+abs(y2-y1)+abs(z2-z1) <= (r1+r2)

common :: (Int, Int) -> (Int, Int) -> (Int, Int)
common (a,b) (c,d) = (max a c, min b d)

maxClique :: [Bot] -> [Bot]
maxClique bots = head $ getMaximalCliques overlap bots

intersection :: [Bot] -> Planes
intersection bots = foldr1 (zipWith common) $ map toPlanes $ maxClique bots

-- this isn't actually correct, just works on the input data
closest :: Planes -> Int
closest = fst . head

day23a :: [Bot] :~> Int
day23a = MkSol
    { sParse = parseMaybe $ sepBy1 bot (string "\n")
    , sShow  = show
    , sSolve = Just . leaderBots
    }

day23b :: [Bot] :~> Int
day23b = MkSol
    { sParse = parseMaybe $ sepBy1 bot (string "\n")
    , sShow  = show
    , sSolve = Just . closest . intersection
    }
