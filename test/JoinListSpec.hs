module JoinListSpec where

import Test.Hspec
import HomeworkSeven.JoinList
import HomeworkSeven.Sized
import HomeworkSeven.Scrabble

spec :: Spec
spec = do
    describe "tag" $ do
        it "returns EmptyList" $
            tag (EmptyList :: JoinList String Integer) `shouldBe` []
        it "returns a string" $
            tag (Single "Hello" (1 :: Integer)) `shouldBe` "Hello"
        it "returns a string" $
            tag (Append "Helloworld" (Single "Hello" (1 :: Integer)) (Single "world" (2 :: Integer)))
            `shouldBe` "Helloworld"
    describe "(+++)" $ do
        it "returns a JoinList with two EmptyList components" $
            (+++) (EmptyList :: JoinList String Integer) (EmptyList :: JoinList String Integer)
            `shouldBe` Append "" EmptyList EmptyList
        it "returns a JoinList with one EmptyList component and a Single one" $
            (+++) (Single "Hello" (1 :: Integer)) (EmptyList :: JoinList String Integer)
            `shouldBe` Append "Hello" (Single "Hello" 1) EmptyList
        it "returns a JoinList with two Single components" $
            (+++) (Single "Hello" (1 :: Integer)) (Single " world" (2 :: Integer))
            `shouldBe` Append "Hello world" (Single "Hello" 1) (Single " world" (2 :: Integer))
    describe "indexJ" $ do
        it "returns Nothing when looking for an invalid index" $
            indexJ (-1) exOneData `shouldBe` Nothing
        it "returns Nothing when looking for an index out of bounds" $
            indexJ 10 exOneData `shouldBe` Nothing
        it "returns Just 1" $
            indexJ 0 exOneData `shouldBe` Just 1
        it "returns Just 3" $
            indexJ 2 exOneData `shouldBe` Just 3
    describe "dropJ" $ do
        it "returns an EmptyList" $
            dropJ 1 (Single (Size 1) (1 :: Int)) `shouldBe` EmptyList
        it "returns a Single element" $
            dropJ 1 (Append (Size 2) (Single (Size 1) (1 :: Int)) (Single (Size 1) (2 :: Int)))
            `shouldBe` Single (Size 1) (2 :: Int)
        it "returns two elements" $
            dropJ 1 (Append (Size 3) 
                        (Single (Size 1) (1 :: Int)) 
                        (Append (Size 2) 
                            (Single (Size 1) (2 :: Int)) (Single (Size 1) (3 :: Int))
                        )
                    )
            `shouldBe` Append (Size 2) (Single (Size 1) (2 :: Int)) (Single (Size 1) (3 :: Int))
        it "returns a Single element" $
            dropJ 2 (Append (Size 3) 
                        (Single (Size 1) (1 :: Int)) 
                        (Append (Size 2) 
                            (Single (Size 1) (2 :: Int)) (Single (Size 1) (3 :: Int))
                        )
                    )
            `shouldBe` Single (Size 1) (3 :: Int)
    describe "takeJ" $ do
        it "returns an EmptyList" $
            takeJ 0 (Single (Size 1) (1 :: Int)) `shouldBe` EmptyList
        it "returns a Single element" $
            takeJ 1 (Append (Size 2) (Single (Size 1) (1 :: Int)) (Single (Size 1) (2 :: Int)))
            `shouldBe` Append (Size 1) (Single (Size 1) (1 :: Int)) EmptyList
        it "returns two elements" $
            takeJ 2 (Append (Size 3) 
                        (Single (Size 1) (1 :: Int)) 
                        (Append (Size 2) 
                            (Single (Size 1) (2 :: Int)) (Single (Size 1) (3 :: Int))
                        )
                    )
            `shouldBe` Append (Size 2) 
                            (Single (Size 1) (1 :: Int)) 
                            (Append (Size 1) (Single (Size 1) (2 :: Int)) EmptyList)
        it "returns a Single element" $
            takeJ 3 (Single (Size 1) (3 :: Int))
            `shouldBe` Single (Size 1) (3 :: Int)
    describe "scoreLine" $ do
        it "returns a single element with score 9" $
            scoreLine "yay " `shouldBe` Single (Score 9) "yay "
        it "returns a single element with score 9" $
            scoreLine "haskell" `shouldBe` Single (Score 14) "haskell"
        it "returns true" $
            (+++) (scoreLine "yay ") (scoreLine "haskell!")
            `shouldBe`
            Append (Score 23) (Single (Score 9) "yay ") (Single (Score 14) "haskell!")
    describe "scoreAndSizeLine" $ do
        it "returns a score of 9 and size of 1" $
            scoreAndSizeLine "yay " `shouldBe` Single (Score 9, Size 1) "yay "
        it "returns a score of 14 and size of 1" $
            scoreAndSizeLine "haskell!" `shouldBe` Single (Score 14, Size 1) "haskell!"