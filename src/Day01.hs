{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE ViewPatterns #-}

module Day01 where

import           Conduit
import           Control.Monad (void, (>=>))
import qualified Data.IntSet   as IS
import           Data.Maybe    (fromMaybe)
import qualified Data.Text     as T
import           Safe          (readMay)
import           System.IO     (FilePath)


fileFrequencyShifts :: MonadResource m => FilePath -> ConduitT a Int m ()
fileFrequencyShifts path =
       sourceFile path
    .| decodeUtf8LenientC
    .| linesUnboundedC
    .| mapC (fromMaybe 0 . parseFrequency)
  where
    parseFrequency = T.uncons >=> \(sign, T.unpack -> n) -> if
        | sign == '+' -> readMay n
        | sign == '-' -> negate <$> readMay n
        | otherwise   -> Nothing

fileFrequency :: MonadResource m => FilePath -> ConduitT a Void m Int
fileFrequency path = fileFrequencyShifts path .| sumC

fileFrequencies :: MonadResource m => FilePath -> ConduitT a Int m ()
fileFrequencies path = do
    shifts <- fileFrequencyShifts path .| sinkList
    yieldMany (cycle shifts) .| scanlC (+) 0 .| (dropC 1 >> mapC id)

fileDuplicateFrequencies :: MonadResource m => FilePath -> ConduitT a Int m ()
fileDuplicateFrequencies path =
    fileFrequencies path .| void (mapAccumWhileC go IS.empty) .| concatC
  where
    go n set
        | n `IS.member` set = Right (set, Just n)
        | otherwise         = Right (IS.insert n set, Nothing)


answer1 :: IO Int
answer1 = runConduitRes $ fileFrequency "inputs/day01.txt"

answer2 :: IO (Maybe Int)
answer2 = runConduitRes $ fileDuplicateFrequencies "inputs/day01.txt" .| headC
