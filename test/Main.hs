{-# LANGUAGE OverloadedStrings #-}

module Main where

import Maple.Builder (BuilderM, cleanRunBuilder, finishNode, runBuilder, startNode, token)
import Maple.Green (GreenNode (gnWidth), greenNodeEq)
import Test.Hspec

kNumber, kStar, kMul :: Int
kNumber = 1
kStar = 2
kMul = 3

twoTimesTwo :: BuilderM ()
twoTimesTwo = do
    startNode kMul
    token kNumber "2"
    token kStar "*"
    token kNumber "2"
    finishNode

main :: IO ()
main = hspec $ do
    describe "NodeCache" $ do
        it "builds equal trees for cached input" $ do
            (first, cache) <- cleanRunBuilder twoTimesTwo
            (second, _) <- runBuilder cache twoTimesTwo
            greenNodeEq first second `shouldReturn` True
    describe "GreenSize" $ do
        it "size of green tree = bytes" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            gnWidth n `shouldBe` 3
