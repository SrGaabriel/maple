module Maple.Cache where

import qualified Data.HashMap.Strict as HM

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO (evaluate)
import Maple.Green (Green, GreenNode (GreenNode), GreenToken (GreenToken), NodeKey (NodeKey), RawKind, TokenKey (TokenKey), childKey, greenWidth)
import Symbolize (Symbol)

data NodeCache = NodeCache
    { nodeCache :: IORef (HM.HashMap NodeKey GreenNode)
    , tokenCache :: IORef (HM.HashMap TokenKey GreenToken)
    }

newCache :: IO NodeCache
newCache = NodeCache <$> newIORef HM.empty <*> newIORef HM.empty

node :: NodeCache -> RawKind -> [Green] -> IO GreenNode
node cache kind children = do
    keys <- mapM childKey children
    let key = NodeKey kind keys
    m <- readIORef (nodeCache cache)
    case HM.lookup key m of
        Just existing -> pure existing
        Nothing -> do
            let width = sum $ map greenWidth children
            new <- evaluate $ GreenNode kind width children
            writeIORef (nodeCache cache) (HM.insert key new m)
            pure new

token :: NodeCache -> RawKind -> Symbol -> Int -> IO GreenToken
token cache kind symbol width = do
    let key = TokenKey kind symbol
    m <- readIORef (tokenCache cache)
    case HM.lookup key m of
        Just existing -> pure existing
        Nothing -> do
            new <- evaluate $ GreenToken kind symbol width
            writeIORef (tokenCache cache) (HM.insert key new m)
            pure new
