module Maple.Position where

type Pos = Int

-- Exclusive end
data Range = Range Pos Pos deriving (Show, Eq)

posInRange :: Pos -> Range -> Bool
posInRange p (Range s e) = p <= s && s < e

isSubrange :: Range -> Range -> Bool
isSubrange (Range s1 e1) (Range s2 e2) = (s2 >= s1) && (e2 < e1)