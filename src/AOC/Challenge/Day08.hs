{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day08
-- License     : BSD3

module AOC.Challenge.Day08 (
    day08a
  , day08b
  , tree
  ) where

import AOC.Prelude
import Data.Tree

tree :: [Int] -> Tree [Int]
tree = fst . goTree
  where goTree (0:nmeta:rest) = (Node (take nmeta rest) [], drop nmeta rest)
        goTree (n:nmeta:rest) = (Node (take nmeta rest') children, drop nmeta rest')
          where (children, rest') = (iterate goForest ([], rest)) !! n
        goForest (trees, rest) = (tree:trees, rest')
          where (tree, rest') = goTree rest

sumMeta :: Tree [Int] -> Int
sumMeta = sum . concat . flatten

day08a :: [Int] :~> Int
day08a = MkSol
    { sParse = Just . map read . words
    , sShow  = show
    , sSolve = Just . sumMeta . tree
    }

day08b :: _ :~> _
day08b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just
    }
