{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Maple.Print where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Builder (Builder, fromString, fromText, singleton, toLazyText)
import Maple.Ast (SyntaxKind (fromRaw))
import Maple.Green (Green (GNode, GToken), GreenNode (GreenNode, gnChildren, gnKind, gnWidth), GreenToken (GreenToken, gtKind, gtSymbol), RawKind)
import Maple.Position (Range (Range))
import Maple.Red (Red (RNode, RToken), RedNode (RedNode, rnGreen, rnRange), RedToken (RedToken, rtGreen, rtRange), materializeChildren)
import Symbolize (unintern)

type KindPrinter = RawKind -> Text

rawKind :: KindPrinter
rawKind = T.pack . show

showKind :: forall k. (SyntaxKind k, Show k) => KindPrinter
showKind = T.pack . show . fromRaw @k

prettyGreen :: KindPrinter -> GreenNode -> Text
prettyGreen pk = render . greenNodeLines pk 0

prettyRed :: KindPrinter -> RedNode -> Text
prettyRed pk = render . redNodeLines pk 0

greenNodeLines :: KindPrinter -> Int -> GreenNode -> Builder
greenNodeLines pk depth GreenNode{gnKind, gnWidth, gnChildren} =
    line depth (kind pk gnKind <> singleton ' ' <> fromString (show gnWidth))
        <> foldMap (greenLines pk (depth + 1)) gnChildren

greenLines :: KindPrinter -> Int -> Green -> Builder
greenLines pk depth (GNode n) = greenNodeLines pk depth n
greenLines pk depth (GToken GreenToken{gtKind, gtSymbol}) =
    line depth (kind pk gtKind <> singleton ' ' <> tokenText (unintern gtSymbol))

redNodeLines :: KindPrinter -> Int -> RedNode -> Builder
redNodeLines pk depth red@RedNode{rnGreen = GreenNode{gnKind}, rnRange} =
    line depth (kind pk gnKind <> range rnRange)
        <> foldMap (redLines pk (depth + 1)) (materializeChildren red)

redLines :: KindPrinter -> Int -> Red -> Builder
redLines pk depth (RNode n) = redNodeLines pk depth n
redLines pk depth (RToken RedToken{rtGreen = GreenToken{gtKind, gtSymbol}, rtRange}) =
    line depth (kind pk gtKind <> range rtRange <> singleton ' ' <> tokenText (unintern gtSymbol))

kind :: KindPrinter -> RawKind -> Builder
kind pk = fromText . pk

range :: Range -> Builder
range (Range s e) = singleton '@' <> fromString (show s) <> ".." <> fromString (show e)

tokenText :: Text -> Builder
tokenText = fromString . show

line :: Int -> Builder -> Builder
line depth b = fromText (T.replicate depth "  ") <> b <> singleton '\n'

render :: Builder -> Text
render = TL.toStrict . toLazyText

