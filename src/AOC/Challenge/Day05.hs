{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day05
-- License     : BSD3

module AOC.Challenge.Day05 (
    day05a
  , day05b
  , react
  , reactWithout
  ) where

import AOC.Prelude
import Debug.Trace

react :: String -> String
react = foldr reactWithHead []
  where reactWithHead last (head:rest) | toLower last == toLower head && last /= head = rest
        reactWithHead last (head:rest) = last:head:rest
        reactWithHead last [] = [last]

reactWithout :: Char -> String -> String
reactWithout omitted = foldr reactWithHead []
  where reactWithHead last (head:rest) | toLower last == toLower head && last /= head = rest
        reactWithHead last (head:rest) | last == omitted || toLower last == omitted = head:rest
        reactWithHead last (head:rest) = last:head:rest
        reactWithHead last [] | last == omitted || toLower last == omitted = []
        reactWithHead last [] = [last]

day05a :: String :~> Int
day05a = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = Just . length . react
    }

day05b :: String :~> Int
day05b = MkSol
    { sParse = Just
    , sShow  = show
    , sSolve = \input -> Just $ minimum $ map length $ map (\one -> reactWithout one input) ['a'..'z']
    }
