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

import Debug.Trace

tree :: [Int] -> Tree [Int]
tree = fst . goTree
  where goTree (0:nmeta:rest) = (Node (take nmeta rest) [], drop nmeta rest)
        goTree (n:nmeta:rest) = (Node (take nmeta rest') (reverse children), drop nmeta rest')
          where (children, rest') = (iterate goForest ([], rest)) !! n
        goForest (trees, rest) = (tree:trees, rest')
          where (tree, rest') = goTree rest

sumMeta :: Tree [Int] -> Int
sumMeta = sum . concat . flatten

treeValue :: Tree [Int] -> Int
treeValue = foldTree nodeValue
  where nodeValue meta [] = sum meta
        nodeValue meta childrenValue = sum $ map (\n -> if n-1 < length childrenValue then childrenValue !! (n-1) else 0) meta

day08a :: [Int] :~> Int
day08a = MkSol
    { sParse = Just . map read . words
    , sShow  = show
    , sSolve = Just . sumMeta . tree
    }

day08b :: [Int] :~> Int
day08b = MkSol
    { sParse = Just . map read . words
    , sShow  = show
    , sSolve = Just . treeValue . tree
    }
