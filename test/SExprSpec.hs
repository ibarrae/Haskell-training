module SExprSpec where

import Test.Hspec
import Data.Char
import HomeworkTen.AParser
import HomeworkEleven.SExpr

spec :: Spec 
spec = do
    describe "zeroOrMore" $ do
        it "responds with an empty list" $
            runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" 
            `shouldBe` Just("", "abcdeFGh")
        it "responds with a list" $
            runParser (zeroOrMore (satisfy isUpper)) "XYZdeFGh"
            `shouldBe` Just("XYZ", "deFGh")
    describe "oneOrMore" $ do
        it "responds with Nothing" $
            runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
            `shouldBe` Nothing
        it "responds with a list" $
            runParser (oneOrMore (satisfy isUpper)) "ADdeFGh"
            `shouldBe` Just("AD", "deFGh")
    describe "spaces" $ do
        it "responds with an empty String" $
            runParser spaces "AbCd" `shouldBe` Just("","AbCd")
        it "responds with spaces" $
            runParser spaces "   A" `shouldBe` Just("   ","A")
    describe "ident" $ do
        it "responds with Nothing" $
            runParser ident "2bad" `shouldBe` Nothing
        it "responds with Nothing" $
            runParser ident "" `shouldBe` Nothing
        it "responds with valid characters" $
            runParser ident "a1b2c 123" `shouldBe` Just("a1b2c"," 123")
        it "responds with valid characters" $
            runParser ident "foo33fA" `shouldBe` Just("foo33fA","")
        it "responds with valid characters" $
            runParser ident "bar" `shouldBe` Just("bar","")
    describe "parseSExpr" $ do
        it "responds with an Integer Atom" $
            runParser parseSExpr "5" `shouldBe` Just(A (N 5),"")
        it "responds with an Ident Atom" $
            runParser parseSExpr "foo3" `shouldBe` Just(A (I "foo3"),"")
        it "responds with a combination" $
            runParser parseSExpr "(bar (foo) 3 5 874)" 
            `shouldBe` 
            Just (Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)],"")
        it "responds with a combination" $
            runParser parseSExpr "(((lambda x (lambda y (plus x y))) 3) 5)"
            `shouldBe`
            Just (
                    Comb [
                        Comb [
                            Comb [
                                A (I "lambda"), A (I "x"), Comb [
                                    A (I "lambda"), A (I "y"), Comb [
                                        A (I "plus"), A (I "x"), A (I "y")
                                    ]
                                ]
                            ]
                            , A (N 3)
                        ]
                        , A (N 5)
                    ]
                 , "")
        it "responds with Nothing" $
            runParser parseSExpr "(test" `shouldBe` Nothing

