module HomeworkFour.HigherOrder where

import Data.List
import Data.Bool

data Tree a
 = Leaf
 | Node Integer (Tree a) a (Tree a)
 deriving (Show, Eq)

fun1 :: [Integer] -> Integer 
fun1 [] = 1
fun1 (x:xs)
 | even x    = (x - 2) * fun1 xs
 | otherwise = fun1 xs

fun2 :: Integer -> Integer 
fun2 1 = 0
fun2 n
 | even n = n + fun2 (n `div` 2)
 | otherwise = fun2 (3 * n + 1)

fun1' :: [Integer] -> Integer
fun1' = product . map (2-) . filter even

fun2' :: Integer -> Integer
fun2' number = 
    let f e = bool (3 * e + 1) (e `div` 2 ) (even e)
    in sum . filter even . takeWhile (>1) $ iterate f number

foldTree :: [a] -> Tree a
foldTree = foldr insertBinaryTree Leaf

insertBinaryTree :: a -> Tree a -> Tree a
insertBinaryTree a Leaf = Node 0 Leaf a Leaf
insertBinaryTree a (Node h left value right) 
 | leftHeight < rightHeight = Node (rightHeight + 1) (insertBinaryTree a left) value right
 | otherwise = Node (h + 1) left value (insertBinaryTree a right)
 where leftHeight = getHeight left
       rightHeight = getHeight right

getHeight :: Tree a -> Integer
getHeight Leaf = 0
getHeight (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor = foldl xorFun False
    where xorFun a b = (a || b) && not (a && b)

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\e acc -> f e : acc) []

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = sieveNumbers
    where numbers = [1..n]
          ignoredNumbers = generateIgnoredNumbers n
          sieveFilter a = a `notElem` ignoredNumbers
          sieveNumbers = filter (< n) $ map (\e-> 2 * e + 1) $ filter sieveFilter numbers

generateIgnoredNumbers :: Integer -> [Integer]
generateIgnoredNumbers n = 
    let numbers = [1..n]
    in nub $ foldl (\acc e -> acc ++ generateNumbers e) [] numbers    
    where generateNumbers i = filter (<= n) $ foldl (\acc e -> i + e + 2 * i * e : acc) [] [i..n]
