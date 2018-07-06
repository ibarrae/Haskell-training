{-# OPTIONS_GHC -fno-warn-orphans #-}

module PersonSpec where

import MonoidExercise.Person

import Test.Hspec
import Test.QuickCheck.Arbitrary
import Test.Hspec.QuickCheck

instance Arbitrary Age where
    arbitrary = Age <$> arbitrary

instance Arbitrary Person where
    arbitrary = Person <$> arbitrary <*> arbitrary

spec :: Spec
spec =
    describe "meets the following requeriments to be a monoid" $ do
        prop "identity" $ \x  ->
            x `mappend` mempty == mempty `mappend` (x :: Person)
        prop "commutability" $ \a b c  ->
            (a `mappend` b) `mappend` (c :: Person)
                ==
            a `mappend` (b `mappend` c)