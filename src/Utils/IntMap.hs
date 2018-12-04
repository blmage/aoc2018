module Utils.IntMap
    (
      maximumPair
    , maximumPairBy
    ) where

import           Data.Foldable      (maximumBy)
import           Data.Function      (on)
import qualified Data.IntMap.Strict as IM


maximumPairBy :: (a -> a -> Ordering) -> IM.IntMap a -> Maybe (Int, a)
maximumPairBy f map
    | IM.null map = Nothing
    | otherwise   = Just $ maximumBy (f `on` snd) $ IM.toList map

maximumPair :: Ord a => IM.IntMap a -> Maybe (Int, a)
maximumPair = maximumPairBy compare
