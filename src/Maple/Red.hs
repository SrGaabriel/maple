{-# LANGUAGE NamedFieldPuns #-}

module Maple.Red where

import Maple.Green (GreenNode (gnWidth), GreenToken)
import Maple.Position (Pos, Range (Range))

data Red
    = RNode RedNode
    | RToken RedToken

data RedNode
    = RedNode
    { rnGreen :: GreenNode
    , rnParent :: Maybe (RedNode)
    , rnPos :: !Pos
    }

data RedToken
    = RedToken
    { rtGreen :: GreenToken
    , rtPos :: !Pos
    , rtParent :: RedNode
    , rtIndex :: !Int
    }

rnRange :: RedNode -> Range
rnRange (RedNode{rnPos, rnGreen}) = Range rnPos (rnPos + gnWidth rnGreen)
