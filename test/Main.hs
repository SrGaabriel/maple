module Main where

import Maple.Cache (NodeCache, mkCache, node, token)
import Maple.Green (Green (GNode, GToken), GreenNode, greenEq)
import Symbolize (intern)
import Test.Hspec

kNumber, kStar, kMul :: Int
kNumber = 1
kStar = 2
kMul = 3

twoTimesTwo :: NodeCache -> IO GreenNode
twoTimesTwo cache = do
    a <- GToken <$> token cache kNumber (intern "2") 1
    b <- GToken <$> token cache kStar (intern "*") 1
    a' <- GToken <$> token cache kNumber (intern "2") 1
    node cache kMul [a, b, a']

main :: IO ()
main = hspec $ do
    describe "NodeCache" $ do
        it "builds equal trees for cached input" $ do
            cache <- mkCache
            first <- GNode <$> twoTimesTwo cache
            second <- GNode <$> twoTimesTwo cache
            greenEq first second `shouldReturn` True