module HomeworkThree.Golf where

import Data.List

skip :: [a] -> [[a]]
skip = generateSkips

generateSkips :: [element] -> [[element]]
generateSkips elements = map (retrieveElements . getMultipleIndexes indexedElements) indexes 
    where indexedElements = indexElements elements
          indexes = [1..length indexedElements]

indexElements :: [element] -> [(Int, element)]
indexElements = zip [1..]          

getMultipleIndexes :: [(Int, element)] -> Int -> [(Int, element)]
getMultipleIndexes _ 0 = []
getMultipleIndexes elements index = filter (\e -> (fst e `mod` index) == 0) elements

retrieveElements :: [(Int, element)] -> [element]
retrieveElements = map snd

localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:rest) = calculateLocal x y z ++ localMaxima (y:z:rest)
localMaxima _ = []

calculateLocal :: Integer -> Integer -> Integer -> [Integer]
calculateLocal first second third
 | second > first && third < second = [second]
 | otherwise = []

histogram :: [Int] -> String
histogram [] = ""
histogram numbers = generateHistogram ocurrences (maximum ocurrences)
 where ocurrences = findOcurrences [0..9] numbers

generateHistogram :: [Int] -> Int -> String
generateHistogram _ 0 = "==========\n0123456789\n"
generateHistogram ocurrences line = generateHistogramLine ocurrences line ++ generateHistogram ocurrences (line - 1)

generateHistogramLine :: [Int] -> Int -> String
generateHistogramLine ocurrences line = map (`validateOcurrence` line) ocurrences ++ "\n"

validateOcurrence :: Int -> Int -> Char
validateOcurrence ocurrence line 
 | ocurrence >= line = '*'
 | otherwise = ' '

findOcurrences :: [Int] -> [Int] -> [Int]
findOcurrences x y = map (`findIntegerOcurrences` y) x

findIntegerOcurrences :: Int -> [Int] -> Int
findIntegerOcurrences number numbers = length $ elemIndices number numbers