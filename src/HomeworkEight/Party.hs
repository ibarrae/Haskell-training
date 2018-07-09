module HomeworkEight.Party where

import HomeworkEight.Employee
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ ef) (GL l f) = GL (e:l) (ef + f)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1 gl2 = case gl1 `compare` gl2 of
    GT -> gl1
    _  -> gl2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f initialValue (Node x treeList) =
    --apply function to
    --  node value as first par
    --  a list of something as second param
    --  returns a something
    f x (map (treeFold f initialValue) treeList)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel b@(Emp _ bf) [] = (GL [b] bf, GL [] 0)
nextLevel b@(Emp _ bf) gls = 
    let withBoss = foldr (\e acc -> fst e `mappend` acc) (GL [] 0) gls
        withoutBoss = foldr (\e acc -> snd e `mappend` acc) (GL [] 0) gls
        bgl = GL [b] bf
        in (bgl `mappend` withoutBoss, withBoss `mappend` withoutBoss)

maxFun :: Tree Employee -> GuestList
maxFun t = 
    let gls = treeFold nextLevel (mempty, mempty) t
    in fst gls

main :: IO [()]
main = 
    do
        file <- readFile "src/HomeworkEight/company.txt"
        printGuestList $ maxFun (read file :: Tree Employee)