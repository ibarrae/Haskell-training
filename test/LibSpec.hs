module LibSpec where

import Test.Hspec
import Lib

spec :: Spec
spec = 
    describe "add" $ 
        it "returns the sums of two numbers" $ 
              add 4 5 `shouldBe` 9