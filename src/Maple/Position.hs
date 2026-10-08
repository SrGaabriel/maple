module Maple.Position where
import qualified Data.Text as T

type Pos = Int

-- Exclusive end
data Range = Range Pos Pos deriving (Show, Eq)

posInRange :: Pos -> Range -> Bool
posInRange p (Range s e) = s <= p && p < e

posRowCol :: T.Text -> Pos -> (Int, Int)
posRowCol text pos = go 0 0 0
  where
    go row col idx
      | idx >= pos = (row, col)
      | idx >= T.length text = (row, col)
      | otherwise =
          let c = T.index text idx
          in if c == '\n'
             then go (row + 1) 0 (idx + 1)
             else go row (col + 1) (idx + 1)