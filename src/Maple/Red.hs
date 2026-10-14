{-# LANGUAGE NamedFieldPuns #-}

module Maple.Red where

import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Vector.Strict as V
import Maple.Green (Green (GNode, GToken), GreenNode (GreenNode, gnChildren, gnKind, gnWidth), GreenToken (gtKind, gtSymbol), RawKind, greenWidth)
import Maple.Position (Range (Range))
import Symbolize (unintern)

data Red
    = RNode RedNode
    | RToken RedToken

data RedNode
    = RedNode
    { rnGreen :: GreenNode
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

mkRedNode :: GreenNode -> Maybe RedNode -> Range -> Int -> RedNode
mkRedNode green parent range index =
    let node =
            RedNode
                { rnGreen = green
                , rnParent = parent
                , rnRange = range
                , rnIndex = index
                , rnChildren = buildChildren node
                }
    in node

materializeRoot :: GreenNode -> RedNode
materializeRoot green@(GreenNode{gnWidth}) = mkRedNode green Nothing (Range 0 gnWidth) 0

materializeChildren :: RedNode -> V.Vector Red
materializeChildren = rnChildren

nodeKind :: RedNode -> RawKind
nodeKind = gnKind . rnGreen

tokenKind :: RedToken -> RawKind
tokenKind = gtKind . rtGreen

tokenText :: RedToken -> Text
tokenText = unintern . gtSymbol . rtGreen

buildChildren :: RedNode -> V.Vector Red
buildChildren
    parent@( RedNode
                { rnGreen = GreenNode{gnChildren}
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
                                RNode $ mkRedNode greenNode (Just parent) (Range prev end) i
                    in end `seq` (end, child)
                )
                start
                (V.indexed gnChildren)
