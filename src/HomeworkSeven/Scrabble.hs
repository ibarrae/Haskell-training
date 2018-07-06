module HomeworkSeven.Scrabble where

import Data.Char

newtype Score 
    = Score Int
    deriving (Show, Eq)

instance Monoid Score where
    mempty = Score 0
    mappend a b = Score (getScore a + getScore b)

getScore :: Score -> Int
getScore (Score s) = s

score :: Char -> Score
score c 
    | inside "AEILNORST" = Score 1
    | inside "DG"        = Score 2
    | inside "BCPM"      = Score 3
    | inside "FHVWY"     = Score 4
    | inside "K"         = Score 5
    | inside "JX"        = Score 8
    | inside "QZ"        = Score 10   
    | otherwise          = Score 0
    where inside = elem . toUpper $ c

scoreString :: String -> Score
scoreString = foldr (\e acc -> acc `mappend` score e) (Score 0)