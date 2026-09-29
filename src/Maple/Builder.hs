{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Maple.Builder where

import Control.Monad.State.Strict
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector.Strict as V
import qualified Maple.Cache as Cache
import Maple.Green (Green (GNode, GToken), GreenNode, RawKind)
import Symbolize (intern)

data BuilderState = BuilderState
    { bsCache :: !Cache.NodeCache
    , bsOpen :: ![Frame] -- innermost:outers
    , bsRoots :: ![Green]
    }

data Frame = Frame
    { frKind :: !RawKind
    , frChildren :: ![Green] -- reverse
    , frCount :: !Int
    }

newtype BuilderT m a = BuilderT (StateT BuilderState m a)
    deriving
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , MonadTrans
        , MonadState BuilderState
        , MonadFail
        )

type BuilderM = BuilderT IO

startNode :: (MonadIO m) => RawKind -> BuilderT m ()
startNode kind = do
    let frame =
            Frame
                { frKind = kind
                , frChildren = []
                , frCount = 0
                }
    modify' (\st -> st{bsOpen = frame : bsOpen st})

token :: (MonadIO m) => RawKind -> Text -> BuilderT m ()
token kind text = do
    cache <- gets bsCache
    (tok, cache') <- liftIO $ Cache.token cache kind (intern text) (T.length text)

    modify'
        ( \st@BuilderState{bsOpen} -> do
            let (lst@Frame{frChildren, frCount} : frRest) = bsOpen
            st
                { bsOpen =
                    lst
                        { frChildren = GToken tok : frChildren
                        , frCount = frCount + 1
                        }
                        : frRest
                , bsCache = cache'
                }
        )

finishNode :: (MonadIO m) => BuilderT m ()
finishNode = do
    ~BuilderState{bsOpen = Frame{frKind, frCount, frChildren} : frUpper, bsRoots, bsCache} <- get
    let vChildren = V.fromListN frCount frChildren
    (node, bsCache') <- liftIO $ Cache.node bsCache frKind vChildren

    let (bsOpen', bsRoots') = case frUpper of
            [] -> ([], (GNode node) : bsRoots)
            (parent@Frame{frChildren = pChildren} : xs) ->
                ( parent{frChildren = (GNode node) : pChildren} : xs
                , bsRoots
                )

    modify'
        ( \st ->
            st
                { bsOpen = bsOpen'
                , bsRoots = bsRoots'
                , bsCache = bsCache'
                }
        )

runBuilderT :: (MonadIO m) => Cache.NodeCache -> BuilderT m () -> m (GreenNode, Cache.NodeCache)
runBuilderT cache (BuilderT builder) = do
    (_, st) <- runStateT builder initialState
    case bsRoots st of
        [GNode root] -> pure (root, bsCache st)
        _ -> error "runBuilderT: expected exactly one root"
  where
    initialState =
        BuilderState
            { bsCache = cache
            , bsOpen = []
            , bsRoots = []
            }

cleanRunBuilderT :: (MonadIO m) => BuilderT m () -> m (GreenNode, Cache.NodeCache)
cleanRunBuilderT = runBuilderT Cache.mkCache

runBuilder :: Cache.NodeCache -> BuilderM () -> IO (GreenNode, Cache.NodeCache)
runBuilder = runBuilderT

cleanRunBuilder :: BuilderM () -> IO (GreenNode, Cache.NodeCache)
cleanRunBuilder = cleanRunBuilderT
