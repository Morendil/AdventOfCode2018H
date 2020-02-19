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

type Bot = (Int, Int, Int, Int)

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
    , sSolve = Just . length
    }
