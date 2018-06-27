module LogAnalysisSpec where

import Test.Hspec
import LogAnalysis

spec :: Spec
spec = do
    describe "parseMessage" $ do
        it "returns an error message" $ 
            parseMessage "E 2 562 help help" `shouldBe` LogMessage (Error 2) 562 "help help"
        it "returns an information message" $ 
            parseMessage "I 29 la la la" `shouldBe` LogMessage Info 29 "la la la"
        it "returns a warning message" $ 
            parseMessage "W 5 la li lu" `shouldBe` LogMessage Warning 5 "la li lu"
        it "returns an unknown message" $ 
            parseMessage "This is not in the right format" `shouldBe` Unknown "This is not in the right format"
        it "returns an unknown message" $ 
            parseMessage "Hello world" `shouldBe` Unknown "Hello world"    
        it "returns an empty message" $ 
            parseMessage "" `shouldBe` Empty
    describe "generateMessage" $ do
        it "returns Empty if empty" $ 
            generateMessage [] `shouldBe` Empty
        it "returns False if invalid message" $
            generateMessage (words "123abc") `shouldBe` Unknown "123abc"
        it "returns LogMessage if valid error message" $
            generateMessage (words "E 2 562 help help") `shouldBe` LogMessage (Error 2) 562 "help help"
        it "returns True if valid info message" $
            generateMessage (words "I 5 info info") `shouldBe` LogMessage Info 5 "info info"
        it "returns True if valid warning message" $
            generateMessage (words "W 10 warn warn") `shouldBe` LogMessage Warning 10 "warn warn"
    describe "toInfoMessage" $ do
        it "returns an Unknown message" $
            toInfoMessage ["I", "hello", "world"] `shouldBe` Unknown "I hello world"
        it "returns a LogMessage" $
            toInfoMessage ["5", "hello", "world", "from", "this", "side"] `shouldBe` LogMessage Info 5 "hello world from this side"
    describe "toWarningMessage" $ do
        it "returns an Unknown message" $
            toWarningMessage ["W", "hello", "world"] `shouldBe` Unknown "W hello world"
        it "returns a LogMessage" $
            toWarningMessage ["25", "hello", "world", "from", "this", "side"] `shouldBe` LogMessage Warning 25 "hello world from this side"
    describe "toErrorMessage" $ do
        it "returns an Unknown message" $
            toErrorMessage ["I", "25", "hello", "world"] `shouldBe` Unknown "I 25 hello world"
        it "returns a LogMessage" $
            toErrorMessage ["5", "10",  "hello", "world", "from", "this", "side"] `shouldBe` LogMessage (Error 5) 10 "hello world from this side"
    describe "insert" $ do
        it "returns a Leaf" $
            build [] `shouldBe` Leaf
        it "returns a Tree with a left node" $
            build [LogMessage Info 15 "head", LogMessage (Error 25) 7 "left"] `shouldBe` Node (Node Leaf (LogMessage (Error 25) 7 "left") Leaf) (LogMessage Info 15 "head") Leaf
        it "returns a Tree with a right node" $
            build [LogMessage Info 15 "first", LogMessage Info 7 "second", LogMessage (Error 25) 5 "third"] `shouldBe` Node (Node (Node Leaf (LogMessage (Error 25) 5 "third") Leaf) (LogMessage Info 7 "second") Leaf) (LogMessage Info 15 "first") Leaf
    describe "inOrder" $ do
        it "returns an empty list" $
            inOrder Leaf `shouldBe` []
        it "returns a list of ordered log messages" $
            inOrder (Node (Node (Node Leaf (LogMessage (Error 25) 5 "third") Leaf) (LogMessage Info 7 "second") Leaf) (LogMessage Info 15 "first") Leaf) `shouldBe` [LogMessage (Error 25) 5 "third", LogMessage Info 7 "second", LogMessage Info 15 "first"]
        it "returns a list of ordered log messages" $
           inOrder (Node (Node (Node Leaf (LogMessage (Error 25) 5 "third") Leaf) (LogMessage Info 7 "second") Leaf) (LogMessage Info 15 "first") (Node Leaf (LogMessage Warning 77 "fourth") Leaf)) `shouldBe` [LogMessage (Error 25) 5 "third", LogMessage Info 7 "second", LogMessage Info 15 "first", LogMessage Warning 77 "fourth"]
    describe "whatWentWrong" $ do
        it "returns an empty list" $
            whatWentWrong [] `shouldBe` []
        it "returns an empty list if there are no log messages with severity over 50" $
            whatWentWrong [LogMessage Info 15 "first", LogMessage Info 7 "second", LogMessage (Error 25) 5 "third"] `shouldBe` []
        it "returns a list of strings" $
            whatWentWrong [LogMessage Info 59 "first", LogMessage Info 7 "second", LogMessage (Error 51) 6 "third", LogMessage (Error 75) 6 "fourth"] `shouldBe` ["third", "fourth"]