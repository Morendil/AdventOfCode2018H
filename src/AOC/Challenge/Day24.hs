{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day24
-- License     : BSD3

module AOC.Challenge.Day24 (
    day24a
  , day24b
  ) where

import AOC.Prelude hiding (group, option)
import Text.ParserCombinators.ReadP

bludgeoning = do string "bludgeoning"; return Bludgeoning
radiation = do string "radiation"; return Radiation
slashing = do string "slashing"; return Slashing
cold = do string "cold"; return Cold
fire = do string "fire"; return Fire
aType = choice [bludgeoning, radiation, slashing, cold, fire]

immunities_ = do
    string "immune to "
    types <- sepBy1 aType (string ", ")
    return (\group -> group {immunities = types})

weaknesses_ = do
    string "weak to "
    types <- sepBy1 aType (string ", ")
    return (\group -> group {weaknesses = types})

options = between (char '(') (string ") ") (sepBy1 (choice [immunities_, weaknesses_]) (string "; "))

group = do
    units <- int
    string " units each with "
    hitPoints <- int
    string " hit points "
    optionals <- option [] options
    string "with an attack that does "
    damage <- int
    char ' '
    itsType <- aType
    string " damage at initiative "
    init <- int    
    let attack = Attack damage itsType init
        result = Group "" units hitPoints attack [] []
    return $ foldr id result optionals

anArmy = do
    name <- many1 $ satisfy (/= ':')
    char ':'; char '\n'
    groups <- sepBy1 group (char '\n')
    char '\n'
    return $ map (\group -> group {army = name}) groups

armies = do
    army1 <- anArmy
    char '\n'
    army2 <- anArmy
    return $ army1 ++ army2

data AttackType = Bludgeoning | Radiation | Slashing | Cold | Fire deriving (Eq, Show)
data Attack = Attack {damage :: Int, attackType :: AttackType, initiative :: Int} deriving (Eq, Show)

data Group = Group {army :: String, units :: Int, hitPoints :: Int, attack :: Attack, weaknesses :: [AttackType], immunities :: [AttackType]} deriving (Eq, Show)

effectivePower :: Group -> Int
effectivePower group = (units group) * (damage $ attack group)

effectiveHitpoints :: Group -> Int
effectiveHitpoints group = (units group) * (hitPoints group)

dealDamage :: Group -> Group -> Int
dealDamage atk def = if immune then 0 else if weak then 2 * dmg else dmg
  where dmg = effectivePower atk
        what = attackType $ attack atk
        immune = elem what $ immunities def
        weak = elem what $ weaknesses def

damageMatrix :: [Group] -> ([[Int]],[[Int]])
damageMatrix groups = (cross army1 army2, cross army2 army1)
  where (army1, army2) = partition (\group -> (army group) == (army $ head groups)) groups
        cross g1 g2 = [[dealDamage i1 i2 | i2 <- g2] | i1 <- g1]

toPair :: [Group] -> ([Group], [Group])
toPair groups = partition (\group -> (army group) == (army $ head groups)) groups

attackGroup :: Group -> Group -> Group
attackGroup attacker defender = defender {units = max 0 $ units defender - (damageSuffered `div` hitPoints defender)}
  where damageSuffered = dealDamage attacker defender

war :: [Group] -> Int
war = sum . map units . untilStable . iterate fight

fight :: [Group] -> [Group]
fight groups = cleanup $ foldl (doFight order) groups $ byInitiative
  where byInitiative = sortOn (Down . initiative . attack . snd) (indexed groups)
        order = targetSelection groups

doFight :: [(Int, Int)] -> [Group] -> (Int, Group) -> [Group]
doFight order groups (index, _) = if isJust defender
    then replace (fromJust defender) result groups
    else groups
  where defender = lookup index order
        result = attackGroup (groups !! index) (groups !! fromJust defender)

cleanup :: [Group] -> [Group]
cleanup = filter (\group -> units group > 0)

targetSelection :: [Group] -> [(Int, Int)]
targetSelection groups = foldl (pickTarget groups) [] $ sortBy selectionOrder (indexed groups)
  where selectionOrder = mappend (comparing (Down . effectivePower . snd)) (comparing (Down . initiative . attack . snd))

pickTarget :: [Group] -> [(Int, Int)] -> (Int, Group) -> [(Int, Int)]
pickTarget groups picks (atkIndex, attacker) = if null candidates then picks else picks ++ [choose $ head candidates]
  where indexedTargets = indexed groups
        targetOrder = mconcat $ map comparing [(Down . dealDamage attacker . snd), (Down . effectivePower . snd), (Down . initiative . attack . snd)]
        picked target = isJust $ find (\(_,index) -> index == target) picks
        candidates = filter (\(index, group) -> (not.picked) index && opposing group && (dealDamage attacker group) > 0) (sortBy targetOrder indexedTargets)
        opposing target = army target /= army attacker
        choose (index, target) = (atkIndex, index)

boost :: String -> Int -> [Group] -> [Group]
boost name amount = map (\g -> if army g == name then g {attack = (attack g) {damage = (damage.attack) g + amount}} else g)

summary armies = doBoth $ toPair armies
  where doBoth (a, b) = (doOne a, doOne b)
        doOne groups = if null groups then 0 else (sum $ map units $ groups)

day24a :: [Group] :~> Int
day24a = MkSol
    { sParse = parseMaybe armies
    , sShow  = show
    , sSolve = Just . war
    }

-- binary searched by hand, so this will not work in the general case
day24b :: [Group] :~> Int
day24b = MkSol
    { sParse = parseMaybe armies
    , sShow  = show
    , sSolve = Just . sum . map units . untilStable . iterate fight . boost "Immune System" 31
    }
