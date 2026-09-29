module Maple.Position where

type Pos = Int

-- Exclusive end
data Range = Range Pos Pos deriving (Show, Eq)

posInRange :: Pos -> Range -> Bool
posInRange p (Range s e) = s <= p && p < e
