{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Maple.Green where

import Control.Exception (evaluate)
import Data.Hashable (Hashable)
import qualified Data.Vector.Strict as V
import GHC.Generics (Generic)
import GHC.StableName (StableName, makeStableName)
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

keyName :: a -> IO (StableName a)
keyName a = evaluate a >>= makeStableName

childKey :: Green -> IO ChildKey
childKey (GNode n) = KNode <$> (keyName n)
childKey (GToken t) = KToken <$> (keyName t)

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
    , gnChildren :: !(V.Vector Green)
    }

greenEq :: Green -> Green -> IO Bool
greenEq (GNode n1) (GNode n2) = greenNodeEq n1 n2
greenEq (GToken t1) (GToken t2) = greenTokenEq t1 t2
greenEq _ _ = pure False

greenNodeEq :: GreenNode -> GreenNode -> IO Bool
greenNodeEq x y = (==) <$> (keyName x) <*> (keyName y)

greenTokenEq :: GreenToken -> GreenToken -> IO Bool
greenTokenEq x y = (==) <$> (keyName x) <*> (keyName y)
