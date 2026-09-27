{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module Maple.Green where

import Control.Exception (evaluate)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import GHC.StableName
import Symbolize (Symbol)

type RawKind = Int

data ChildKey
    = KNode (StableName GreenNode)
    | KToken (StableName GreenToken)
    deriving (Eq, Generic, Hashable)

data NodeKey = NodeKey !RawKind [ChildKey]
    deriving (Eq, Generic, Hashable)

data TokenKey = TokenKey !RawKind !Symbol
    deriving (Eq, Generic, Hashable)

childKey :: Green -> IO ChildKey
childKey (GNode n) = KNode <$> (evaluate n >>= makeStableName)
childKey (GToken t) = KToken <$> (evaluate t >>= makeStableName)

greenWidth :: Green -> Int
greenWidth (GNode n) = gnWidth n
greenWidth (GToken t) = gtWidth t

data Green
    = GToken GreenToken
    | GNode GreenNode

data GreenToken = GreenToken
    { gtKind :: !RawKind
    , gtSymbol :: !Symbol
    , gtWidth :: !Int
    }

data GreenNode = GreenNode
    { gnKind :: !RawKind
    , gnWidth :: !Int
    , gnChildren :: [Green]
    }
