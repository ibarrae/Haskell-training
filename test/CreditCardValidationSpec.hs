module CreditCardValidationSpec where

import Test.Hspec
import CreditCardValidation

spec :: Spec
spec = do
    describe "isValidNumber" $ do
        it "returns True if the credit card number is valid" $
              isValidNumber 4012888888881881 `shouldBe` True
        it "returns False if the credit card number is invalid" $
              isValidNumber 4012888888881882 `shouldBe` False
    describe "toDigits" $ do
        it "returns a list of digits from a number" $
              toDigits 12345 `shouldBe` [1, 2, 3, 4, 5]
        it "returns an empty list of digits from an incorrect number" $
              toDigits 0 `shouldBe` []
    describe "toDigitsRev" $ do
        it "returns a list of reversed digits from a number" $
              toDigitsRev 12345 `shouldBe` [5, 4, 3, 2, 1]
        it "returns an empty list of digits from an incorrect number" $
              toDigitsRev 0 `shouldBe` []
    describe "doubleEveryOther" $ do
        it "returns a list of every other element doubled" $
              doubleEveryOther [1, 2, 3, 4, 5] `shouldBe` [1, 4, 3, 8, 5]
        it "returns an empty list" $
              doubleEveryOther [] `shouldBe` []
        it "returns the same list" $
              doubleEveryOther [33] `shouldBe` [33]              
    describe "sumDigits" $ do
        it "returns a sumatory of the digits" $ 
              sumDigits [1, 2, 33, 44, 55] `shouldBe` 27
        it "returns zero" $ 
              sumDigits [] `shouldBe` 0