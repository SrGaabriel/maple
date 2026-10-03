{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Data (Data)
import Data.Maybe (isJust, isNothing)
import Maple.Ast (AstNode, OfKind (OfKind), SyntaxKind (toRaw), cast)
import Maple.Builder (BuilderM, cleanRunBuilder, finishNode, runBuilder, startNode, token)
import Maple.Green (GreenNode (gnWidth), greenNodeEq)
import Maple.Print (prettyGreen, prettyRed, showKind)
import Maple.Red (RedNode, materializeRoot)
import Test.Hspec

data Kind = KNumber | KStar | KMul
    deriving (Eq, Show, Enum, Data)
    deriving anyclass (SyntaxKind)

newtype MulExpr = MulExpr RedNode
    deriving (AstNode) via (OfKind 'KMul)

newtype NumberExpr = NumberExpr RedNode
    deriving (AstNode) via (OfKind 'KNumber)

kNumber, kStar, kMul :: Int
kNumber = toRaw KNumber
kStar = toRaw KStar
kMul = toRaw KMul

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
    describe "Ast" $ do
        it "casts a node via derived OfKind" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            let root = materializeRoot n
            isJust (cast root :: Maybe MulExpr) `shouldBe` True
            isNothing (cast root :: Maybe NumberExpr) `shouldBe` True
    describe "Print" $ do
        it "pretty prints a green tree" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            prettyGreen (showKind @Kind) n
                `shouldBe` "KMul 3\n  KNumber \"2\"\n  KStar \"*\"\n  KNumber \"2\"\n"
        it "pretty prints a red tree" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            prettyRed (showKind @Kind) (materializeRoot n)
                `shouldBe` "KMul@0..3\n  KNumber@0..1 \"2\"\n  KStar@1..2 \"*\"\n  KNumber@2..3 \"2\"\n"
