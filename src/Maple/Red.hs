module Maple.Red where

import Maple.Green (GreenNode)

data RedNode a
    = RedNode
    { rnGreen :: GreenNode
    , rnParent :: Maybe (RedNode a)
    , rnChildren :: [RedNode a]
    }
