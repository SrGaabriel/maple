{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Data (Data)
import Data.Maybe (isJust, isNothing)
import GHC.Generics (Generic, Generically (Generically))
import Maple.Ast (AstNode (syntax), AstToken, OfKind (OfKind), SyntaxKind (toRaw), TokenOfKind (TokenOfKind), cast, childToken, children)
import Maple.Builder (BuilderM, cleanRunBuilder, finishNode, runBuilder, startNode, token)
import Maple.Green (GreenNode (gnWidth), greenNodeEq)
import Maple.Print (prettyGreen, prettyRed, showKind)
import Maple.Red (RedNode, RedToken, materializeRoot, nodeKind)
import Test.Hspec
import qualified Data.Vector.Strict as V

data Kind = KNumber | KStar | KMul | KLit
    deriving (Eq, Show, Enum, Data)
    deriving anyclass (SyntaxKind)

newtype MulExpr = MulExpr RedNode
    deriving (AstNode) via (OfKind 'KMul)

newtype NumberExpr = NumberExpr RedNode
    deriving (AstNode) via (OfKind 'KNumber)

newtype LitExpr = LitExpr RedNode
    deriving (AstNode) via (OfKind 'KLit)

data Expr = ELit LitExpr | EMul MulExpr
    deriving (Generic)
    deriving (AstNode) via (Generically Expr)

newtype Number = Number RedToken
    deriving (AstToken) via (TokenOfKind 'KNumber)

newtype Star = Star RedToken
    deriving (AstToken) via (TokenOfKind 'KStar)

data Atom = ANumber Number | AStar Star
    deriving (Generic)
    deriving (AstToken) via (Generically Atom)

kNumber, kStar, kMul, kLit :: Int
kNumber = toRaw KNumber
kStar = toRaw KStar
kMul = toRaw KMul
kLit = toRaw KLit

nested :: BuilderM ()
nested = do
    startNode kMul
    twoTimesTwo
    token kStar "*"
    startNode kLit
    token kNumber "3"
    finishNode
    finishNode

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
        it "casts sum nodes via Generically" $ do
            (n, _) <- cleanRunBuilder nested
            let exprs = children (materializeRoot n) :: V.Vector Expr
                tag (ELit _) = "lit"
                tag (EMul _) = "mul"
            map tag (V.toList exprs) `shouldBe` ["mul", "lit" :: String]
            map (nodeKind . syntax) (V.toList exprs) `shouldBe` [kMul, kLit]
        it "casts sum tokens via Generically" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            let describe' (Just (ANumber _)) = "number"
                describe' (Just (AStar _)) = "star"
                describe' Nothing = "none"
            describe' (childToken (materializeRoot n) :: Maybe Atom) `shouldBe` ("number" :: String)
    describe "Print" $ do
        it "pretty prints a green tree" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            prettyGreen (showKind @Kind) n
                `shouldBe` "KMul 3\n  KNumber \"2\"\n  KStar \"*\"\n  KNumber \"2\"\n"
        it "pretty prints a red tree" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            prettyRed (showKind @Kind) (materializeRoot n)
                `shouldBe` "KMul@0..3\n  KNumber@0..1 \"2\"\n  KStar@1..2 \"*\"\n  KNumber@2..3 \"2\"\n"
