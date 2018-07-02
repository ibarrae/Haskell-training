{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module CalcSpec where

import Test.Hspec
import Calc
import StackVM

spec :: Spec
spec = do
    describe "eval" $ do
        it "returns a number" $
            eval (Lit 0) `shouldBe` 0
        it "returns a number" $
            eval (Add (Lit 5) (Lit 10)) `shouldBe` 15
        it "returns a number" $
            eval (Mul (Lit 5) (Lit 4)) `shouldBe` 20
        it "returns a number" $
            eval (Mul
                    (Add (Lit 1) (Lit 3))
                    (Lit 3))
            `shouldBe` 12
    describe "evalStr" $ do
        it "returns Nothing" $
            evalStr "" `shouldBe` Nothing
        it "returns Just 4" $
            evalStr "2+2" `shouldBe` Just 4 
        it "returns Nothing" $
            evalStr "2%2" `shouldBe` Nothing
        it "returns Just 28" $
            evalStr "(2+3)*4" `shouldBe` Just 20
    describe "compile" $ do
        it "returns nothing" $
            compile "" `shouldBe` Nothing
        it "returns nothing" $
            compile "3+" `shouldBe` Nothing
        it "returns a list with 3 elements" $
            compile "3+5" `shouldBe` Just [PushI 3, PushI 5, AddVM]
        it "returns a list with 5 elements" $
            compile "(1+4)*8" `shouldBe` Just [PushI 1, PushI 4, AddVM, PushI 8, MulVM]