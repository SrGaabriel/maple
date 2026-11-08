{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Data (Data)
import Data.Maybe (isJust, isNothing)
import Data.Vector.Strict qualified as V
import GHC.Generics (Generic, Generically (Generically))
import Maple.Ast (AstNode (syntax), AstToken, OfKind (OfKind), SyntaxKind (toRaw), TokenOfKind (TokenOfKind), cast, childToken, children)
import Maple.Builder (BuilderM, checkpoint, cleanRunBuilder, finishNode, runBuilder, startNode, startNodeAt, token)
import Maple.Green (GreenNode (gnWidth), greenNodeEq)
import Maple.Position (Span (), TreeId (TreeId))
import Maple.Print (prettyGreen, prettyRed, showKind)
import Maple.Red (Red (RNode, RToken), RedNode, RedToken, materializeChildren, materializeRoot, nodeKind, nodeSpan, tokenSpan)
import Test.Hspec

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

tree0 :: TreeId
tree0 = TreeId 0

allSpans :: RedNode -> [Span]
allSpans node = nodeSpan node : concatMap go (V.toList (materializeChildren node))
  where
    go (RNode n) = allSpans n
    go (RToken t) = [tokenSpan t]

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
            let root = materializeRoot tree0 n
            isJust (cast root :: Maybe MulExpr) `shouldBe` True
            isNothing (cast root :: Maybe NumberExpr) `shouldBe` True
        it "casts sum nodes via Generically" $ do
            (n, _) <- cleanRunBuilder nested
            let exprs = children (materializeRoot tree0 n) :: V.Vector Expr
                tag (ELit _) = "lit"
                tag (EMul _) = "mul"
            map tag (V.toList exprs) `shouldBe` ["mul", "lit" :: String]
            map (nodeKind . syntax) (V.toList exprs) `shouldBe` [kMul, kLit]
        it "casts sum tokens via Generically" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            let describe' (Just (ANumber _)) = "number"
                describe' (Just (AStar _)) = "star"
                describe' Nothing = "none"
            describe' (childToken (materializeRoot tree0 n) :: Maybe Atom) `shouldBe` ("number" :: String)
    describe "Checkpoint" $ do
        it "wraps only the children added since the checkpoint" $ do
            (n, _) <- cleanRunBuilder $ do
                startNode kMul
                token kNumber "1"
                cp <- checkpoint
                startNode kLit
                token kNumber "2"
                finishNode
                token kStar "*"
                startNodeAt cp kMul
                finishNode
                finishNode
            prettyGreen (showKind @Kind) n
                `shouldBe` "KMul 3\n  KNumber \"1\"\n  KMul 2\n    KLit 1\n      KNumber \"2\"\n    KStar \"*\"\n"
    describe "Print" $ do
        it "pretty prints a green tree" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            prettyGreen (showKind @Kind) n
                `shouldBe` "KMul 3\n  KNumber \"2\"\n  KStar \"*\"\n  KNumber \"2\"\n"
        it "pretty prints a red tree" $ do
            (n, _) <- cleanRunBuilder twoTimesTwo
            prettyRed (showKind @Kind) (materializeRoot tree0 n)
                `shouldBe` "KMul@0..3\n  KNumber@0..1 \"2\"\n  KStar@1..2 \"*\"\n  KNumber@2..3 \"2\"\n"
