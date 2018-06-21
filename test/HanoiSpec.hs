module HanoiSpec where

import Test.Hspec
import Hanoi

spec :: Spec
spec = do
    describe "getMovements" $ do
        it "returns the movements for solving hanoi towers with 3 pegs and 3 disks" $ 
            getMovements 3 "a" "b" "c" `shouldBe` [("a","c"),("a","b"),("c","b"),("a","c"),("b","a"),("b","c"),("a","c")]
        it "returns the movements for solving hanoi towers with 3 pegs and 1 disks" $ 
            getMovements 1 "a" "b" "c" `shouldBe` [("a","c")]
        it "returns the movements for solving hanoi towers with 3 pegs and 0 disks" $ 
            getMovements 0 "a" "b" "c" `shouldBe` []
    describe "getMovementsWithExtraPeg" $ do
        it "returns the movements for solving hanoi towers with 4 pegs and 4 disks" $ 
            getMovementsWithExtraPeg 4 "a" "b" "c" "d" `shouldBe` [("a","d"),("a","b"),("d","b"),("a","c"),("a","d"),("c","d"),("b","c"),("b","d"),("c","d")]
        it "returns the movements for solving hanoi towers with 4 pegs and 1 disks" $ 
            getMovementsWithExtraPeg 1 "a" "b" "c" "d" `shouldBe` [("a","d")]
        it "returns the movements for solving hanoi towers with 4 pegs and 0 disks" $ 
            getMovementsWithExtraPeg 0 "a" "b" "c" "d" `shouldBe` []
