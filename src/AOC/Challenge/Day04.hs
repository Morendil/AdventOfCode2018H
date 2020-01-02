{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day04
-- License     : BSD3

module AOC.Challenge.Day04 (
    day04a
  , day04b
  ) where

import AOC.Prelude
import Text.ParserCombinators.ReadP hiding (get)
import qualified Data.Map as M

import Debug.Trace

type Sleeps = M.Map Int [Int]
data Time = Time Int Int Int Int deriving (Eq, Show)
data What = Shift Int | Sleep | Wake deriving (Eq, Show)
data Event = Event Time What deriving (Eq, Show)
instance Ord Event where
  compare (Event (Time m1 d1 hr1 min1) _) (Event (Time m2 d2 hr2 min2) _) = compare (m1,d1,hr1,min1) (m2,d2,hr2,min2)

timestamp =
  mkTime <$> string "[1518-" <*> int <*> string "-" <*> int <*> string " " <*> int <*> string ":" <*> int <*> string "] "
  where mkTime _ month _ day _ hour _ minute _ = Time month day hour minute

what = choice [beginShift, fallAsleep, wakeUp]
  where beginShift = mkShift <$> string "Guard #" <*> int <*> string " begins shift"
        mkShift _ guard _ = Shift guard
        fallAsleep = Sleep <$ string "falls asleep"
        wakeUp = Wake <$ string "wakes up"

event = Event <$> timestamp <*> what

recordSleeps :: [Event] -> Sleeps
recordSleeps = (flip evalState) (Nothing, 0) . foldlM insertWithState M.empty . sort

insertWithState :: Sleeps -> Event -> State (Maybe Int, Int) Sleeps
insertWithState sleeps (Event (Time _ _ hr min) (Shift guard)) = do
  put (Just guard, if hr == 0 then min else 0)
  return sleeps
insertWithState sleeps (Event (Time _ _ hr min) Sleep) = do
  (which, lastMin) <- get
  put (which, if hr == 0 then min else 0)
  return sleeps
insertWithState sleeps (Event (Time _ _ hr min) Wake) = do
  (which, lastMin) <- get
  let (Just guard) = which
  put (which, if hr == 0 then min else 0)
  return $ foldr (\min map -> M.alter (Just . maybe [min] (min:)) guard map) sleeps [lastMin..min-1]

mostAsleep :: Sleeps -> Int
mostAsleep = fst . M.foldrWithKey (\g mins prev@(pg,pmins) -> if mins > pmins then (g,mins) else prev) (-1, 0) . M.map length

strategy1 :: Sleeps -> Int
strategy1 sleeps = (mostMinute $ fromJust (M.lookup which sleeps)) * which
  where which = mostAsleep sleeps
        mostMinute = head . last . sortOn length . (group.sort)

mostFrequent :: Sleeps -> (Int, [Int])
mostFrequent = M.foldrWithKey (\g mins prev@(pg,pmins) -> if length mins > length pmins then (g,mins) else prev) (-1, []) . M.map (head.sortOn(Down . length).group.sort)

strategy2 :: Sleeps -> Int
strategy2 sleeps = guard * head mins
  where (guard, mins) = mostFrequent sleeps

day04a :: [Event] :~> Int
day04a = MkSol
    { sParse = parseMaybe $ sepBy1 event (string "\n")
    , sShow  = show
    , sSolve = Just . strategy1 . recordSleeps
    }

day04b :: [Event] :~> Int
day04b = MkSol
    { sParse = parseMaybe $ sepBy1 event (string "\n")
    , sShow  = show
    , sSolve = Just . strategy2 . recordSleeps
    }
