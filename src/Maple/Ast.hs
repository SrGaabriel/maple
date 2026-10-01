{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Maple.Ast where

import Data.Data (Data, dataTypeOf, fromConstr, readConstr)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import Maple.Green (GreenNode (GreenNode, gnKind), RawKind)
import Maple.Red (RedNode (RedNode, rnGreen))

class SyntaxKind k where
    toRaw :: k -> RawKind
    default toRaw :: (Enum k) => k -> RawKind
    toRaw = fromEnum

    fromRaw :: RawKind -> k
    default fromRaw :: (Enum k) => RawKind -> k
    fromRaw = toEnum

class AstNode a where
    canCast :: Proxy a -> RawKind -> Bool
    unsafeWrap :: RedNode -> a
    syntax :: a -> RedNode

cast :: forall a. (AstNode a) => RedNode -> Maybe a
cast red@(RedNode{rnGreen = GreenNode{gnKind}})
    | canCast (Proxy @a) gnKind = Just $ unsafeWrap red
    | otherwise = Nothing

kindVal :: forall {kind} (k :: kind). (Typeable k, Data kind) => kind
kindVal =
    case readConstr (dataTypeOf (undefined :: kind)) name of
        Just con -> fromConstr con
        Nothing -> error $ "Maple.Ast.kindVal: no constructor named " <> name
  where
    name = dropWhile (== '\'') . tyConName . typeRepTyCon $ typeRep (Proxy @k)

newtype OfKind (k :: kind) = OfKind RedNode

instance (Typeable k, Data kind, SyntaxKind kind) => AstNode (OfKind (k :: kind)) where
    canCast _ = (== raw)
      where
        raw = toRaw (kindVal @k)
    unsafeWrap = OfKind
    syntax (OfKind n) = n
