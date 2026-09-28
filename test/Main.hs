module Main where

import Maple.Cache (NodeCache, mkCache, node, token)
import Maple.Green (Green (GToken), GreenNode (gnWidth), greenNodeEq)
import Symbolize (intern)
import Test.Hspec
import qualified Data.Vector.Strict as V

kNumber, kStar, kMul :: Int
kNumber = 1
kStar = 2
kMul = 3

twoTimesTwo :: NodeCache -> IO GreenNode
twoTimesTwo cache = do
    a <- GToken <$> token cache kNumber (intern "2") 1
    b <- GToken <$> token cache kStar (intern "*") 1
    a' <- GToken <$> token cache kNumber (intern "2") 1
    node cache kMul (V.fromList [a,b,a'])

main :: IO ()
main = hspec $ do
    describe "NodeCache" $ do
        it "builds equal trees for cached input" $ do
            cache <- mkCache
            first <- twoTimesTwo cache
            second <- twoTimesTwo cache
            greenNodeEq first second `shouldReturn` True
    describe "GreenSize" $ do
        it "size of green tree = bytes" $ do
            cache <- mkCache
            n <- twoTimesTwo cache
            (pure $ gnWidth n == 3) `shouldReturn` True