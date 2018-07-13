module AParserSpec where

import Test.Hspec
import HomeworkTen.AParser

spec :: Spec
spec = do
    describe "first" $ do
        it "returns a tuple" $
            first (+1) ((1,2) :: (Int, Int)) `shouldBe` (2,2)
        it "returns a tuple" $
            first (>0) ((1,2) :: (Int, Int)) `shouldBe` (True,2)
    describe "abParser" $ do
        it "returns Nothing" $
            runParser abParser "axasd" `shouldBe` Nothing
        it "returns Just(('a','b'), rest) " $
            runParser abParser "abcdefg" `shouldBe` Just(('a','b'),"cdefg")
    describe "abParser'" $ do
        it "returns Nothing" $
            runParser abParser' "axasd" `shouldBe` Nothing
        it "returns Just((), rest) " $
            runParser abParser' "abcdefg" `shouldBe` Just((),"cdefg")
    describe "intPair" $
        it "returns Just([12,34], rest)" $
            runParser intPair "12 34" `shouldBe` Just([12,34],"")
    describe "intOrUppercase" $ do
        it "returns Just((), rest)" $
            runParser intOrUppercase "342abcd" `shouldBe` Just((), "abcd")
        it "returns Just((), rest)" $
            runParser intOrUppercase "Xabcd" `shouldBe` Just((),"abcd")
        it "returns Nothing" $
            runParser intOrUppercase "abcd" `shouldBe` Nothing