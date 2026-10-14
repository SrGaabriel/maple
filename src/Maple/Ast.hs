{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}

module Maple.Ast where

import Data.Data (Data, dataTypeOf, fromConstr, readConstr)
import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable, tyConName, typeRep, typeRepTyCon)
import Maple.Green (GreenNode (GreenNode, gnKind), RawKind)
import Maple.Red (Red (RToken, RNode), RedNode (RedNode, rnGreen), RedToken, materializeChildren, tokenKind)
import Data.Vector.Strict (mapMaybe)
import qualified Data.Vector.Strict as V

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

castRed :: (AstNode t) => Red -> Maybe t
castRed (RNode node) = cast node
castRed _ = Nothing

child    :: forall a. AstNode a => RedNode -> Maybe a
child = firstJust castRed . materializeChildren

children :: forall a. AstNode a => RedNode -> V.Vector a
children red = mapMaybe castRed (materializeChildren red)

nthChild :: forall a. AstNode a => Int -> RedNode -> Maybe a
nthChild n = nthJust castRed n . materializeChildren

tokenOf :: (SyntaxKind k) => k -> RedNode -> Maybe RedToken
tokenOf k = firstJust match . materializeChildren
  where
    raw = toRaw k
    match (RToken tok) | tokenKind tok == raw = Just tok
    match _ = Nothing

class AstToken t where
    canCastToken :: Proxy t -> RawKind -> Bool
    unsafeWrapToken :: RedToken -> t
    syntaxToken :: t -> RedToken

castToken :: forall t. (AstToken t) => RedToken -> Maybe t
castToken tok
    | canCastToken (Proxy @t) (tokenKind tok) = Just $ unsafeWrapToken tok
    | otherwise = Nothing

childToken :: (AstToken t) => RedNode -> Maybe t
childToken = firstJust castTokenRed . materializeChildren

childTokens :: (AstToken t) => RedNode -> V.Vector t
childTokens = mapMaybe castTokenRed . materializeChildren

nthToken :: (AstToken t) => Int -> RedNode -> Maybe t
nthToken n = nthJust castTokenRed n . materializeChildren

castTokenRed :: (AstToken t) => Red -> Maybe t
castTokenRed (RToken tok) = castToken tok
castTokenRed _ = Nothing

firstJust :: (a -> Maybe b) -> V.Vector a -> Maybe b
firstJust f = V.foldr (\x rest -> maybe rest Just (f x)) Nothing

nthJust :: (a -> Maybe b) -> Int -> V.Vector a -> Maybe b
nthJust f n xs
    | n < 0 = Nothing
    | otherwise = V.foldr step (const Nothing) xs n
  where
    step x rest i = case f x of
        Just b
            | i == 0 -> Just b
            | otherwise -> rest (i - 1)
        Nothing -> rest i

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

newtype TokenOfKind (k :: kind) = TokenOfKind RedToken

instance (Typeable k, Data kind, SyntaxKind kind) => AstToken (TokenOfKind (k :: kind)) where
    canCastToken _ = (== raw)
      where
        raw = toRaw (kindVal @k)
    unsafeWrapToken = TokenOfKind
    syntaxToken (TokenOfKind t) = t
