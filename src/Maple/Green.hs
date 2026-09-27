module Maple.Green where

type RawKind = Int

data GreenNode = GreenNode
    { gnKind :: RawKind
    , gnWidth :: !Int
    , gnHash :: !Int
    , gnChildren :: [GreenNode]
    }
