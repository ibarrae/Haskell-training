module HigherOrderSpec where

import Test.Hspec
import HigherOrder

spec :: Spec
spec = do
    describe "fun1'" $ do
        it "returns 1" $
            fun1' [] `shouldBe` fun1 []
        it "returns a number" $
            fun1' [1..10] `shouldBe` fun1 [1..10]
    describe "fun2'" $ do
        it "returns 1" $
            fun2' 1 `shouldBe` fun2 1
        it "returns a number" $
            fun2' 3 `shouldBe` fun2 3
    describe "foldTree" $ do
        it "returns a Leaf" $
            foldTree "" `shouldBe` (Leaf :: Tree Char)
        it "returns a tree" $
            foldTree "ABC" 
                `shouldBe`
            (Node 2 Leaf 'C' (Node 1 Leaf 'B' (Node 0 Leaf 'A' Leaf)) :: Tree Char)
    describe "insertBinaryTree" $ do
        it "returns a node with two leafs" $
            insertBinaryTree 5 Leaf `shouldBe` (Node 0 Leaf 5 Leaf :: Tree Int)
        it "returns a node with a left leaf and a rigth node" $
            insertBinaryTree 10 (Node 0 Leaf 5 Leaf) 
            `shouldBe`
            (Node 1 Leaf 5 (Node 0 Leaf 10 Leaf) :: Tree Int)
    describe "xor" $ do
        it "returns False" $
            xor [] `shouldBe` False
        it "returns False" $
            xor [False, False] `shouldBe` False
        it "returns False" $
            xor [False, True, True] `shouldBe` False
        it "returns True" $
            xor [False, True, False] `shouldBe` True
    describe "map'" $ do
        it "returns an empty list" $
            map' (+ 1) ([] :: [Int]) `shouldBe` map (+ 1) []
        it "returns a list" $
            map' (+ 1) ([1..10] :: [Int]) `shouldBe` map (+ 1) [1..10]
    describe "sieveSundaram" $ do
        it "returns an empty list" $
            sieveSundaram 3 `shouldBe` []
        it "returns a list" $
            sieveSundaram 10 `shouldBe` [3, 5, 7]