module Utils.Time
    (
      secondsToTime
    , diffTimeToSeconds
    , diffTimeToTime
    , diffTimeToDailyTime
    ) where

import           Data.Time.Clock

import           Utils           (first3)


secondsToTime :: Integer -> (Integer, Int, Int)
secondsToTime s =
    ( s `div` 3600
    , fromInteger $ s `mod` 3600 `div` 60
    , fromInteger $ s `mod` 3600 `mod` 60
    )

diffTimeToSeconds :: DiffTime -> Integer
diffTimeToSeconds = (`div` 1000000000000) . diffTimeToPicoseconds

diffTimeToTime :: DiffTime -> (Integer, Int, Int)
diffTimeToTime = secondsToTime . diffTimeToSeconds

withDailyDiffTime :: (Int -> a) -> DiffTime -> Maybe a
withDailyDiffTime f (diffTimeToSeconds -> s)
    | s >= 86400 = Nothing
    | otherwise  = Just $ f $ fromInteger s

diffTimeToDailyTime :: DiffTime -> Maybe (Int, Int, Int)
diffTimeToDailyTime = fmap (first3 fromInteger) .
    withDailyDiffTime (secondsToTime . toInteger)
