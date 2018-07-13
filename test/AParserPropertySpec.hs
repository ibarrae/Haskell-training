{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, ScopedTypeVariables, FlexibleContexts #-}

module AParserPropertySpec where

import HomeworkTen.AParser
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "parser meets the following requirements to be a functor" $ do
        prop "identity" $ \x ->
            let a = runParser posInt x
            in fmap id a == id a
        prop "composition" $ \x ->
            let a = runParser posInt x
            in fmap (id . id) a == (fmap id . fmap id) a
    describe "parser meets the following requirements to be an applicative" $ do
        prop "identity" $ \x ->
            let a = runParser posInt x
            in (pure id <*> a) == a
        prop "homomorphism" $ \x y ->
            let left = (<*>) (pure id) (pure x :: Parser Int)
                right = pure (id x)
            in runParser left y == runParser right y
        prop "interchange" $ \x y ->
            let a = functionParser (+ x)
                b = pure x :: Parser Integer
                f = a <*> b
                h = pure ($ x) <*> a
            in runParser f y == runParser h y
        prop "composition" $ \w x y z ->
            let a = functionParser (+w)
                b = functionParser (+x)
                c = pure y :: Parser Integer
                left = a <*> (b <*> c) 
                right = pure (.) <*> a <*> b <*> c
            in runParser left z == runParser right z