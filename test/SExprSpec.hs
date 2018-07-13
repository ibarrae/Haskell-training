module SExprSpec where

import Test.Hspec
import Data.Char
import HomeworkTen.AParser
import HomeworkEleven.SExpr

spec :: Spec 
spec = do
  describe "zeroOrMore" $ do
    context "when the parser does not find any value" $
      it "returns an empty list" $
        runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh" 
        `shouldBe` Just("", "abcdeFGh")
    context "when the parser matches any value" $
      it "responds with a list" $
        runParser (zeroOrMore (satisfy isUpper)) "XYZdeFGh"
        `shouldBe` Just("XYZ", "deFGh")
  describe "oneOrMore" $ do
    context "when the parser does not find any value" $
      it "responds with Nothing" $
        runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
        `shouldBe` Nothing
    context "when the parser matches one or more values" $
      it "responds with a list" $
      runParser (oneOrMore (satisfy isUpper)) "ADdeFGh"
      `shouldBe` Just("AD", "deFGh")
  describe "spaces" $ do
    context "when the string does not contain a white space" $
      it "responds with an empty String" $
        runParser spaces "AbCd" `shouldBe` Just("","AbCd")
    context "when the string does contain a white space" $
      it "responds with spaces" $
        runParser spaces "   A" `shouldBe` Just("   ","A")
  describe "ident" $ do
    context "when the string does not start with a lowercase character" $ do
      it "returns Nothing" $
          runParser ident "2bad" `shouldBe` Nothing
      it "returns Nothing" $
          runParser ident "" `shouldBe` Nothing
    context "when the string start with a lowercase character" $ do
      it "returns valid characters" $
          runParser ident "a1b2c 123" `shouldBe` Just("a1b2c"," 123")
      it "returns valid characters" $
          runParser ident "foo33fA" `shouldBe` Just("foo33fA","")
      it "returns valid characters" $
          runParser ident "bar" `shouldBe` Just("bar","")
  describe "parseSExpr" $ do
    context "when the expression is correct" $ do
      it "returns an Integer Atom" $
        runParser parseSExpr "5" `shouldBe` Just(A (N 5),"")
      it "returns an Ident Atom" $
        runParser parseSExpr "foo3" `shouldBe` Just(A (I "foo3"),"")
      it "returns a combination" $
        runParser parseSExpr "(bar (foo) 3 5 874)" 
        `shouldBe` 
        Just (Comb [A (I "bar"), Comb [A (I "foo")], A (N 3), A (N 5), A (N 874)],"")
      it "returns a combination" $
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
    context "when the expression is invalid" $        
      it "responds with Nothing" $
        runParser parseSExpr "(test" `shouldBe` Nothing

