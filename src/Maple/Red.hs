{-# LANGUAGE NamedFieldPuns #-}

module Maple.Red where

import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Vector.Strict as V
import Maple.Green (Green (GNode, GToken), GreenNode (GreenNode, gnChildren, gnKind, gnWidth), GreenToken (gtKind, gtSymbol), RawKind, greenWidth)
import Maple.Position (Range (Range), Span (Span), TreeId)
import Symbolize (unintern)

data Red
    = RNode RedNode
    | RToken RedToken

data RedNode
    = RedNode
    { rnGreen :: GreenNode
    , rnTree :: !TreeId
    , rnParent :: Maybe (RedNode)
    , rnRange :: !Range
    , rnIndex :: !Int
    , rnChildren :: V.Vector Red
    }

data RedToken
    = RedToken
    { rtGreen :: GreenToken
    , rtRange :: !Range
    , rtParent :: RedNode
    , rtIndex :: !Int
    }

mkRedNode :: TreeId -> GreenNode -> Maybe RedNode -> Range -> Int -> RedNode
mkRedNode tree green parent range index =
    let node =
            RedNode
                { rnGreen = green
                , rnTree = tree
                , rnParent = parent
                , rnRange = range
                , rnIndex = index
                , rnChildren = buildChildren node
                }
    in node

materializeRoot :: TreeId -> GreenNode -> RedNode
materializeRoot tree green@(GreenNode{gnWidth}) = mkRedNode tree green Nothing (Range 0 gnWidth) 0

materializeChildren :: RedNode -> V.Vector Red
materializeChildren = rnChildren

nodeKind :: RedNode -> RawKind
nodeKind = gnKind . rnGreen

tokenKind :: RedToken -> RawKind
tokenKind = gtKind . rtGreen

tokenText :: RedToken -> Text
tokenText = unintern . gtSymbol . rtGreen

tokenTree :: RedToken -> TreeId
tokenTree = rnTree . rtParent

nodeSpan :: RedNode -> Span
nodeSpan RedNode{rnTree, rnRange} = Span rnTree rnRange

tokenSpan :: RedToken -> Span
tokenSpan t = Span (tokenTree t) (rtRange t)

buildChildren :: RedNode -> V.Vector Red
buildChildren
    parent@( RedNode
                { rnGreen = GreenNode{gnChildren}
                , rnTree
                , rnRange = (Range start _)
                }
            ) =
        snd
            $ mapAccumL
                ( \prev (i, green) ->
                    let width = greenWidth green
                        end = prev + width
                        child = case green of
                            GToken greenToken ->
                                RToken
                                    RedToken
                                        { rtRange = Range prev end
                                        , rtIndex = i
                                        , rtParent = parent
                                        , rtGreen = greenToken
                                        }
                            GNode greenNode ->
                                RNode $ mkRedNode rnTree greenNode (Just parent) (Range prev end) i
                    in end `seq` (end, child)
                )
                start
                (V.indexed gnChildren)
