{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day12
-- License     : BSD3

module AOC.Challenge.Day12 (
    day12a
  , day12b
  , trim
  , evolve
  , plantSum
  ) where

import AOC.Prelude hiding (get)
import Text.ParserCombinators.ReadP
import qualified Data.Map as M

type Rule = (String, Char)
type Rules = M.Map String Char

initial = mkState <$> string "initial state: " <*> many1 (choice [char '#', char '.']) <*> string "\n\n"
  where mkState _ state _ = state

rule = mkRule <$> many1 (choice [char '#', char '.']) <*> string " => " <*> get
  where mkRule initial _ final = (initial, final)

specs :: ReadP (String, Rules)
specs = do
  initial_ <- initial
  rules_ <- sepBy1 rule (string "\n")
  return (initial_, M.fromList rules_)

windows :: Int -> String -> [String]
windows n xs | length xs < n = []
windows n s@(x:xs) = take n s : (windows n xs)

evolve :: Rules -> (String, Int) -> (String, Int)
evolve rules (state, offset) = trim (map (\window -> fromMaybe '.' $ M.lookup window rules) $ windows 5 $ "...."++state++"....", offset+2)

trim :: (String, Int) -> (String, Int)
trim (state, offset) = (trimEnd $ drop discard state, offset-discard)
  where discard = length $ takeWhile (=='.') state
        trimEnd = reverse . dropWhile (=='.') . reverse

plantSum :: (String, Int) -> Int
plantSum (state, offset) = sum $ (map (\x -> x-offset)) $ findIndices (=='#') state

resultAfter :: Int -> (String, Rules) -> Int
resultAfter n (initial, rules) = plantSum $ (iterate (evolve rules) (initial, 0)) !! n

resultAfterMany :: Int -> (String, Rules) -> Int
resultAfterMany n (initial, rules) = plantSum finalState
  where states = iterate (evolve rules) (initial, 0)
        converged = fromJust $ firstRepeated $ map fst $ states
        [(_,one), (_,two)] = take 2 $ filter (\state -> fst state == converged) $ states
        prefixLength = length $ takeWhile (\state -> fst state /= converged) states
        -- this would be more complicated if period length were > 1 - but it's 1!
        finalState = (converged, one + (n-prefixLength)*(two-one))

day12a :: (String, Rules) :~> Int
day12a = MkSol
    { sParse = parseMaybe specs
    , sShow  = show
    , sSolve = Just . resultAfter 20
    }

day12b :: (String, Rules) :~> Int
day12b = MkSol
    { sParse = parseMaybe specs
    , sShow  = show
    , sSolve = Just . resultAfterMany 50000000000
    }
