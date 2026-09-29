{-# LANGUAGE NamedFieldPuns #-}

module Maple.Red where

import Data.List (mapAccumL)
import qualified Data.Vector.Strict as V
import Maple.Green (Green (GNode, GToken), GreenNode (GreenNode, gnChildren, gnWidth), GreenToken, greenWidth)
import Maple.Position (Range (Range))

data Red
    = RNode RedNode
    | RToken RedToken

data RedNode
    = RedNode
    { rnGreen :: GreenNode
    , rnParent :: Maybe (RedNode)
    , rnRange :: !Range
    , rnIndex :: !Int
    }

data RedToken
    = RedToken
    { rtGreen :: GreenToken
    , rtRange :: !Range
    , rtParent :: RedNode
    , rtIndex :: !Int
    }

materializeRoot :: GreenNode -> RedNode
materializeRoot green@(GreenNode{gnWidth}) =
    let range = Range 0 gnWidth
    in RedNode
        { rnGreen = green
        , rnRange = range
        , rnParent = Nothing
        , rnIndex = 0
        }

materializeChildren :: RedNode -> V.Vector Red
materializeChildren
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
                                RNode
                                    RedNode
                                        { rnRange = Range prev end
                                        , rnParent = Just parent
                                        , rnGreen = greenNode
                                        , rnIndex = i
                                        }
                    in end `seq` (end, child)
                )
                start
                (V.indexed gnChildren)
