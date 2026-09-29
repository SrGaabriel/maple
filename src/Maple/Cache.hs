module Maple.Cache where

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector.Strict as V

import GHC.IO (evaluate)
import Maple.Green (Green, GreenNode (GreenNode), GreenToken (GreenToken), NodeKey (NodeKey), RawKind, TokenKey (TokenKey), childKey, greenWidth)
import Symbolize (Symbol)

data NodeCache = NodeCache
    { nodeCache :: (HM.HashMap NodeKey GreenNode)
    , tokenCache :: (HM.HashMap TokenKey GreenToken)
    }

mkCache :: NodeCache
mkCache = NodeCache HM.empty HM.empty

node :: NodeCache -> RawKind -> V.Vector Green -> IO (GreenNode, NodeCache)
node cache kind children = do
    keys <- mapM childKey children
    let key = NodeKey kind (V.toList keys)
    let nCache = nodeCache cache
    case HM.lookup key nCache of
        Just existing -> pure (existing, cache)
        Nothing -> do
            let width = V.sum $ V.map greenWidth children
            new <- evaluate $ GreenNode kind width children
            let nCache' = HM.insert key new nCache
            pure
                ( new
                , cache
                    { nodeCache = nCache'
                    }
                )

token :: NodeCache -> RawKind -> Symbol -> Int -> IO (GreenToken, NodeCache)
token cache kind symbol width = do
    let key = TokenKey kind symbol
    let tCache = tokenCache cache
    case HM.lookup key tCache of
        Just existing -> pure (existing, cache)
        Nothing -> do
            new <- evaluate $ GreenToken kind symbol width
            let tCache' = HM.insert key new tCache
            pure
                ( new
                , cache
                    { tokenCache = tCache'
                    }
                )
