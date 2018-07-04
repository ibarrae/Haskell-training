{-# LANGUAGE FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

module HomeworkSeven.JoinList where

import HomeworkSeven.Sized
import HomeworkSeven.Scrabble
import Data.Bool
import HomeworkSeven.Buffer

data JoinList m a
    = EmptyList
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Show, Eq)

instance Monoid m => Monoid (JoinList m a) where
    mempty  = EmptyList
    mappend a b = Append (mappend (tag a) (tag b)) a b

instance Buffer (JoinList (Score, Size) String) where
    toString = concat . jlToList
    fromString s = foldr ((+++) . scoreAndSizeLine) EmptyList (lines s)
    line = indexJ
    replaceLine l s jl = takeJ l jl +++ fromString s +++ dropJ (l + 1) jl
    numLines = getSize . size . snd . tag
    value = getScore . fst . tag

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b = Append (mappend (tag a) (tag b)) a b

tag :: Monoid m => JoinList m a -> m
tag EmptyList = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_) !!? 0         = Just x 
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList EmptyList            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i (Single _ s) = bool Nothing (Just s) (i == 0)
indexJ i (Append m l r)
    | i < 0 || i > csize = Nothing
    | i < lsize          = indexJ i l
    | otherwise          = indexJ (i - lsize) r
    where csize = getSize . size $ m
          lsize = getSize . size . tag $ l
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ EmptyList = EmptyList          
dropJ i s@(Single _ _) = bool EmptyList s (i<=0)
dropJ i c@(Append m l r) 
    | i <= 0     = c
    | i >= csize = EmptyList
    | otherwise  = dropJ (i - lsize) r
    where csize = getSize . size $ m
          lsize = getSize . size . tag $ l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ EmptyList = EmptyList
takeJ i s@(Single _ _) = bool EmptyList s (i>0)
takeJ i jl@(Append m l r)
    | i <= 0     = EmptyList
    | i >= csize = jl
    | otherwise  = (+++) (takeJ i l) (takeJ (i - lsize) r)
    where csize = getSize . size $ m
          lsize = getSize . size . tag $ l

exOneData :: JoinList Size Int
exOneData = 
    let s = Size 1
        in Append (Size 3) (Single s 1) (Append (Size 2) (Single s 2) (Single s 3))

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

scoreAndSizeLine :: String -> JoinList (Score, Size) String
scoreAndSizeLine s = Single (scoreString s, Size 1) s

ssjlData :: JoinList (Score, Size) String
ssjlData = fromString $ unlines
                [ "This buffer is for notes you don't want to save, and for"
                , "evaluation of steam valve coefficients."
                , "To load a different file, type the character L followed"
                , "by the name of the file."
                ]