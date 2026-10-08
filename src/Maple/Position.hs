module Maple.Position where
import qualified Data.Text as T

type Pos = Int

-- Exclusive end
data Range = Range Pos Pos deriving (Show, Eq)

posInRange :: Pos -> Range -> Bool
posInRange p (Range s e) = s <= p && p < e

-- 1 indexed row,col
posRowCol :: T.Text -> Pos -> (Int, Int)
posRowCol text pos = 
  let (row, col) = go 1 1 pos (T.unpack text)
  in (row, col)
  where
    go r c _ [] = (r, c)
    go r c p (x:xs)
      | p <= 0 = (r, c)
      | x == '\n' = go (r + 1) 1 (p - 1) xs
      | otherwise = go r (c + 1) (p - 1) xs