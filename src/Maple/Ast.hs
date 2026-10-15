{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}

module Maple.Ast where

import Control.Applicative ((<|>))
import Data.Data (Data, dataTypeOf, fromConstr, readConstr)
import Data.Maybe (fromMaybe)
import GHC.Generics (C, D, Generic (Rep, from, to), Generically (Generically), K1 (K1), M1 (M1), S, U1, (:*:), (:+:) (L1, R1))
import GHC.TypeLits (ErrorMessage (Text), TypeError)
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

child :: (AstNode a) => RedNode -> Maybe a
child = firstJust castRed . materializeChildren

children :: (AstNode a) => RedNode -> V.Vector a
children red = mapMaybe castRed (materializeChildren red)

nthChild :: (AstNode a) => Int -> RedNode -> Maybe a
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
    canCast = let raw = toRaw (kindVal @k) in \_ -> (== raw)
    unsafeWrap = OfKind
    syntax (OfKind n) = n

newtype TokenOfKind (k :: kind) = TokenOfKind RedToken

instance (Typeable k, Data kind, SyntaxKind kind) => AstToken (TokenOfKind (k :: kind)) where
    canCastToken = let raw = toRaw (kindVal @k) in \_ -> (== raw)
    unsafeWrapToken = TokenOfKind
    syntaxToken (TokenOfKind t) = t

instance (Generic a, GAstNode (Rep a)) => AstNode (Generically a) where
    canCast _ = gCanCast (Proxy @(Rep a))
    unsafeWrap n =
        Generically . to . fromMaybe (error "Maple.Ast.unsafeWrap: no constructor accepts this kind") $
            gWrap n
    syntax (Generically a) = gSyntax (from a)

class GAstNode f where
    gCanCast :: Proxy f -> RawKind -> Bool
    gWrap :: RedNode -> Maybe (f p)
    gSyntax :: f p -> RedNode

instance (GAstNode f) => GAstNode (M1 D c f) where
    gCanCast _ = gCanCast (Proxy @f)
    gWrap = fmap M1 . gWrap
    gSyntax (M1 x) = gSyntax x

instance (GAstNode f) => GAstNode (M1 C c f) where
    gCanCast _ = gCanCast (Proxy @f)
    gWrap = fmap M1 . gWrap
    gSyntax (M1 x) = gSyntax x

instance (GAstNode f, GAstNode g) => GAstNode (f :+: g) where
    gCanCast _ k = gCanCast (Proxy @f) k || gCanCast (Proxy @g) k
    gWrap n = (L1 <$> gWrap n) <|> (R1 <$> gWrap n)
    gSyntax (L1 x) = gSyntax x
    gSyntax (R1 x) = gSyntax x

instance (AstNode a) => GAstNode (M1 S c (K1 i a)) where
    gCanCast _ = canCast (Proxy @a)
    gWrap = fmap (M1 . K1) . cast
    gSyntax (M1 (K1 a)) = syntax a

instance (TypeError ('Text "AstNode via Generically: every constructor must have exactly one field")) => GAstNode U1 where
    gCanCast = undefined
    gWrap = undefined
    gSyntax = undefined

instance (TypeError ('Text "AstNode via Generically: every constructor must have exactly one field")) => GAstNode (f :*: g) where
    gCanCast = undefined
    gWrap = undefined
    gSyntax = undefined

instance (Generic t, GAstToken (Rep t)) => AstToken (Generically t) where
    canCastToken _ = gCanCastToken (Proxy @(Rep t))
    unsafeWrapToken tok =
        Generically . to . fromMaybe (error "Maple.Ast.unsafeWrapToken: no constructor accepts this kind") $
            gWrapToken tok
    syntaxToken (Generically t) = gSyntaxToken (from t)

class GAstToken f where
    gCanCastToken :: Proxy f -> RawKind -> Bool
    gWrapToken :: RedToken -> Maybe (f p)
    gSyntaxToken :: f p -> RedToken

instance (GAstToken f) => GAstToken (M1 D c f) where
    gCanCastToken _ = gCanCastToken (Proxy @f)
    gWrapToken = fmap M1 . gWrapToken
    gSyntaxToken (M1 x) = gSyntaxToken x

instance (GAstToken f) => GAstToken (M1 C c f) where
    gCanCastToken _ = gCanCastToken (Proxy @f)
    gWrapToken = fmap M1 . gWrapToken
    gSyntaxToken (M1 x) = gSyntaxToken x

instance (GAstToken f, GAstToken g) => GAstToken (f :+: g) where
    gCanCastToken _ k = gCanCastToken (Proxy @f) k || gCanCastToken (Proxy @g) k
    gWrapToken t = (L1 <$> gWrapToken t) <|> (R1 <$> gWrapToken t)
    gSyntaxToken (L1 x) = gSyntaxToken x
    gSyntaxToken (R1 x) = gSyntaxToken x

instance (AstToken a) => GAstToken (M1 S c (K1 i a)) where
    gCanCastToken _ = canCastToken (Proxy @a)
    gWrapToken = fmap (M1 . K1) . castToken
    gSyntaxToken (M1 (K1 a)) = syntaxToken a

instance (TypeError ('Text "AstToken via Generically: every constructor must have exactly one field")) => GAstToken U1 where
    gCanCastToken = undefined
    gWrapToken = undefined
    gSyntaxToken = undefined

instance (TypeError ('Text "AstToken via Generically: every constructor must have exactly one field")) => GAstToken (f :*: g) where
    gCanCastToken = undefined
    gWrapToken = undefined
    gSyntaxToken = undefined
