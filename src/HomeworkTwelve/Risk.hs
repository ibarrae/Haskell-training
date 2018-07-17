{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HomeworkTwelve.Risk where

import Control.Monad.Random
import Data.List
import Data.Bool
import Control.Arrow

------------------------------------------------------------
-- Die values

newtype DieValue 
  = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

------------------------------------------------------------
-- Risk
type Army 
  = Int

data Battlefield 
  = Battlefield 
  { attackers :: Army
  , defenders :: Army }
  deriving (Show,Eq,Ord)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield at dt) = 
  let order = sortBy (flip compare)
      attack (DV av, DV dv) (Battlefield as df) = 
        bool (Battlefield (as-1) df) (Battlefield as (df-1)) (av > dv)
      generateDices n = replicateM n die
      validateTroops n = bool 1 n (n > 0)
  in do
    ad <- generateDices $ max 3 (validateTroops at)
    dd <- generateDices $ max 2 (validateTroops dt)
    return $ foldr attack bf $ zip (order ad) (order dd)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf@(Battlefield at dt)
  | at < 2 || dt == 0 = return bf
  | otherwise         = battle bf >>= invade

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  bfs  <- replicateM 1000 $ invade bf
  let wins = length $ filter (\e -> defenders e == 0) bfs
  return (fromIntegral wins / 1000)