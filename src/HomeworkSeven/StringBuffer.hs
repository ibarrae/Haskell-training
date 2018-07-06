{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module HomeworkSeven.StringBuffer where

import HomeworkSeven.Buffer

instance Buffer String where
  toString     = id
  fromString   = id
  line n b     = safeIndex n (lines b)
  replaceLine n l b = unlines (take (n - 1) sw ++ [l] ++ drop n sw)
      where sw = lines b
  numLines     = length . lines
  value        = length . words

safeIndex :: Int -> [a] -> Maybe a
safeIndex n _ | n < 0 = Nothing
safeIndex _ []        = Nothing
safeIndex 0 (x:_)     = Just x
safeIndex n (_:xs)    = safeIndex (n-1) xs