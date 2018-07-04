module FibonacciSpec where

import Test.Hspec
import HomeworkSix.Fibonacci

spec :: Spec
spec = do
    describe "fib" $ do
        it "returns cero" $
            fib 0 `shouldBe` 0
        it "returns one" $
            fib 1 `shouldBe` 1
        it "returns a number" $
            fib 5 `shouldBe` 5
    describe "fibs1" $
        it "returns a list with 10 elements" $
            take 10 fibs1 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
    describe "fibs2" $
        it "returns a list with 10 elements" $
            take 10 fibs2 `shouldBe` [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]
    describe "streamToList" $
        it "returns an string" $
            show (streamRepeat 1 :: Stream Int) 
            `shouldBe` "[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]"
    describe "streamMap" $
        it "returns a string" $
            show (streamMap (+ 1) (streamRepeat 1 :: Stream Int)) 
            `shouldBe` "[2,2,2,2,2,2,2,2,2,2,2,2,2,2,2]"
    describe "streamSeed" $
        it "returns a string" $
            show (streamSeed (+ 1) (0 :: Int)) 
            `shouldBe` "[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]"
    describe "nats" $
        it "returns a string" $
            show nats `shouldBe` "[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14]"
    describe "interleaveStreams" $
        it "returns a string" $
            show (interleaveStreams (streamRepeat 0 :: Stream Int) (streamSeed (+ 1) (0 :: Int)))
            `shouldBe` "[0,0,0,1,0,2,0,3,0,4,0,5,0,6,0]"
    describe "ruler" $
        it "returns a string" $
            show ruler `shouldBe` "[0,1,0,2,0,1,0,3,0,1,0,2,0,1,0]"
    describe "fibStream" $
        it "returns a string with fibonacci numbers" $
            show fibStream `shouldBe` "[0,1,1,2,3,5,8,13,21,34,55,89,144,233,377]"