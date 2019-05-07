{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

sortDies :: [DieValue] -> [DieValue]
sortDies = reverse . sort

zipDies :: ([DieValue], [DieValue]) -> [(DieValue, DieValue)]
zipDies (as, ds) = zip (sortDies as) (sortDies ds)

casualties :: ([DieValue], [DieValue]) -> (Army, Army)
casualties dies = (att, def)
  where
    def = sum - att
    att = length $ filter (\(a, d) -> a > d) zipped
    sum = length zipped
    zipped = zipDies dies

rollArmy :: Army -> Rand StdGen [DieValue]
rollArmy a = replicateM a die

effectiveAttackers :: Battlefield -> Army
effectiveAttackers = min 3 . max 0 . flip (-) 1 . attackers

effectiveDefenders :: Battlefield -> Army
effectiveDefenders = min 2 . max 0 . defenders

rollBattle :: Battlefield -> Rand StdGen ([DieValue], [DieValue])
rollBattle b =
  rollArmy (effectiveAttackers b) >>= \as ->
  rollArmy (effectiveDefenders b) >>= \ds ->
  return (as, ds)

rollCasualties :: Battlefield -> Rand StdGen (Army, Army)
rollCasualties b =
  rollBattle b >>= \dies ->
  return (casualties dies)

applyCasualties :: Battlefield -> (Army, Army) -> Battlefield
applyCasualties (Battlefield a d) (ac, dc) = Battlefield (a - ac) (d - dc)

battle :: Battlefield -> Rand StdGen Battlefield
battle b =
  rollCasualties b >>= \cs ->
  return (applyCasualties b cs)

data Outcome = Inconclusive | Win | Defeat
  deriving (Eq, Show)

outcome :: Battlefield -> Outcome
outcome b
  | (defenders b) == 0 = Win
  | (attackers b) <  2 = Defeat
  | otherwise          = Inconclusive

inconclusive :: Battlefield -> Bool
inconclusive = ((==) Inconclusive) . outcome

win :: Battlefield -> Bool
win = ((==) Win) . outcome

repeatInvade :: Battlefield -> Rand StdGen Battlefield
repeatInvade b
  | inconclusive b = invade b
  | otherwise   = return b

invade :: Battlefield -> Rand StdGen Battlefield
invade b = battle b >>= repeatInvade

invadeN :: Int -> Battlefield -> Rand StdGen [Battlefield]
invadeN n = replicateM n . invade

successRate :: [Battlefield] -> Double
successRate bs = (fromIntegral $ length $ filter win bs) / (fromIntegral $ length bs)

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  invadeN 1000 b >>= \bs ->
  return (successRate bs)
