{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day14
-- License     : BSD3

module AOC.Challenge.Day14 (
    day14a
  , day14b
  , digits
  , improve
  , start
  , lookFor
  ) where

import AOC.Prelude
import Data.List.HT
import qualified Data.Sequence as S

type Recipes = (S.Seq Int, (Int, Int))

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse $ unfoldr digits' n
  where digits' 0 = Nothing
        digits' n = Just $ swap $ n `divMod` 10

fromDigits :: [Int] -> Int
fromDigits = foldl (\a b -> (10*a) + b) 0

start :: Recipes
start = (S.fromList [3,7], (0, 1))

improve :: Recipes -> Recipes
improve (scores, (elfOne, elfTwo)) = (newList, (elfOne', elfTwo'))
  where totalScore = oneScore + twoScore
        oneScore = fromJust $ S.lookup elfOne scores
        twoScore = fromJust $ S.lookup elfTwo scores
        newList = foldl' (S.|>) scores $ digits totalScore
        len = length newList
        elfOne' = (elfOne + 1 + oneScore) `mod` len
        elfTwo' = (elfTwo + 1 + twoScore) `mod` len

after :: Int -> Int
after n = fromDigits $ take 10 $ drop n $ toList $ last $ takeUntil (\scores -> (length $ scores) >= n+10) $ map fst (iterate improve start)

lookFor :: [Int] -> Int
lookFor sought = fixLength $ takeUntil (\s -> (justEnding s == sought) || (offOne s == sought)) $ map fst (iterate improve start)
  where justEnding s = toList $ fromMaybe S.empty $ S.lookup ((S.length s)-(length sought)) (S.tails s)
        offOne s = take (length sought) $ toList $ fromMaybe S.empty $ S.lookup ((S.length s)-(length sought)-1) (S.tails s)
        fixLength s = (if (justEnding $ last s) == sought then length $ last s else (length $ last s) -1) - (length sought)

day14a :: Int :~> Int
day14a = MkSol
    { sParse = Just . read
    , sShow  = show
    , sSolve = Just . after
    }

day14b :: [Int] :~> Int
day14b = MkSol
    { sParse = Just . map digitToInt . strip
    , sShow  = show
    , sSolve = Just . lookFor
    }
