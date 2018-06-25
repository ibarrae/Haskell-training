module GolfSpec where

import Test.Hspec
import Golf

spec :: Spec
spec = do
    describe "skip" $ do
        it "returns a list of lists" $ 
            skip "ABCD" `shouldBe` ["ABCD", "BD", "C", "D"]
        it "returns a list of one list" $ 
            skip [1] `shouldBe` ([[1]] :: [[Integer]])
        it "returns a list of lists" $ 
            skip "hello!" `shouldBe` ["hello!", "el!", "l!", "l", "o", "!"]
        it "returns a list of lists" $ 
            skip [True, False] `shouldBe` [[True, False], [False]]
        it "returns an empty list" $ 
            skip [] `shouldBe` ([] :: [[Int]])
    describe "indexElements" $ do
        it "returns an empty list" $
            indexElements "" `shouldBe` []
        it "returns a list" $
            indexElements "Hi" `shouldBe` [(1, 'H'), (2, 'i')]
    describe "getMultipleIndexes" $ do
        it "returns an empty list" $
            getMultipleIndexes [(1, 'H'), (2, 'i')] 0 `shouldBe` []
        it "returns an empty list" $
            getMultipleIndexes [(1, 'H'), (2, 'i')] 3 `shouldBe` []
        it "returns a list with one element" $
            getMultipleIndexes [(1, 'H'), (2, 'i')] 2 `shouldBe` [(2, 'i')]  
    describe "retrieveElements" $ do
        it "returns an empty list" $
            retrieveElements ([] :: [(Int, Int)]) `shouldBe` []
        it "retuns a string" $
            retrieveElements [(1, 'H'), (2, 'i')] `shouldBe` "Hi"
    describe "localMaxima" $ do
        it "returns an Empty list" $
            localMaxima [] `shouldBe` []
        it "returns an Empty list" $
            localMaxima [1, 2, 3, 4, 5] `shouldBe` []
        it "returns a list with one element" $
            localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
        it "returns a list with two elements" $
            localMaxima [1, 3, 2, 5, 4] `shouldBe` [3, 5]
    describe "histogram" $ do
        it "returns a string" $
            histogram [] `shouldBe` ""
        it "returns a string" $
            histogram [3,5] `shouldBe` "   * *    \n==========\n0123456789\n"
        it "returns a string" $
            histogram [1, 1, 1, 5] `shouldBe` " *        \n *        \n *   *    \n==========\n0123456789\n"
    describe "findIntegerOcurrences" $ do
        it "returns 0" $
            findIntegerOcurrences 1 [] `shouldBe` 0
        it "returns 1" $
            findIntegerOcurrences 1 [1, 3, 4] `shouldBe` 1
        it "returns 2" $
            findIntegerOcurrences 1 [1, 2, 1] `shouldBe` 2
    describe "findOcurrences" $ do
        it "returns a list of numbers" $
            findOcurrences [0] [1..9] `shouldBe` [0]
        it "returns a list of numbers" $
            findOcurrences [0..9] [1, 1, 1, 5] `shouldBe` [0,3,0,0,0,1,0,0,0,0]
    describe "generateHistogramLine" $ do
        it "returns an empty String" $
            generateHistogramLine [] 0 `shouldBe` "\n"
        it "returns a String" $
            generateHistogramLine [3, 4, 5, 7, 6, 7, 5, 3, 2, 1] 7 `shouldBe` "   * *    \n"
    describe "generateHistogram" $ do
        it "returns a String" $
            generateHistogram [0,3,0,0,0,1,0,0,0,0] 3 `shouldBe` " *        \n *        \n *   *    \n==========\n0123456789\n"
        it "returns a String" $
            generateHistogram [] 0 `shouldBe` "==========\n0123456789\n" 