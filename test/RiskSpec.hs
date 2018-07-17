module RiskSpec where

import Test.Hspec
import HomeworkTwelve.Risk
import Control.Monad.Random.Lazy

spec :: Spec
spec =
  describe "battle" $ 
    context "when you evaluate a battlefield" $
      it "responds with a battlefield with less attackers or defenders" $ do
        let bf = Battlefield 3 5
        result <- evalRandIO $ battle bf
        bf `shouldSatisfy` (> result)
