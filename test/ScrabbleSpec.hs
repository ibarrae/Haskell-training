module ScrabbleSpec where

import Test.Hspec
import HomeworkSeven.Scrabble

spec :: Spec
spec = do
    describe "score" $ do
        it "returns a score of 0" $
            score ' ' `shouldBe` Score 0
        it "returns a score of 2" $
            score 'd' `shouldBe` Score 2
        it "returns a score of 1" $
            score 'a' `shouldBe` Score 1
        it "returns a score of 3" $
            score 'p' `shouldBe` Score 3
        it "returns a score of 4" $
            score 'y' `shouldBe` Score 4
        it "returns a score of 5" $
            score 'k' `shouldBe` Score 5
        it "returns a score of 8" $
            score 'x' `shouldBe` Score 8
        it "returns a score of 10" $
            score 'z' `shouldBe` Score 10
    describe "scoreString" $ do
        it "returns a score of 9" $
            scoreString "yay " `shouldBe` Score 9
        it "returns a score of 14" $
            scoreString "haskell!" `shouldBe` Score 14