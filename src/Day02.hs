module Day02 where

import           Conduit
import           Control.Applicative (liftA2)
import           Control.Monad       (msum)
import           Data.Coerce         (coerce)
import           Data.Foldable       (fold)
import           Data.List           (group, nub, sort)
import           Data.Semigroup      (Product (..), Sum (..), (<>))
import qualified Data.Text           as T
import qualified System.IO           (FilePath)

import           Utils               (both, fileLines, numberSource)


answer1 :: IO Int
answer1 = fmap mkChecksum . runConduitRes $
       fileLines "inputs/day02.txt"
    .| mapC (fold . nub . map mkCounts . group . sort . T.unpack)
    .| foldC
  where
    mkCounts = \case
        [_, _]    -> (Just $ Sum @Int 1, Nothing)
        [_, _, _] -> (Nothing, Just $ Sum @Int 1)
        _         -> (Nothing, Nothing)
    mkChecksum = getProduct . uncurry (<>) . both (coerce . fold)

answer2 :: IO (Maybe String)
answer2 = fmap go . runConduitRes $
       fileLines "inputs/day02.txt"
    .| mapC T.unpack
    .| sinkList
  where
    go ids = msum $ liftA2 checkBoxIds ids ids

    checkBoxIds (c : cs) (c' : cs')
        | c == c'   = (c :) <$> checkBoxIds cs cs'
        | cs == cs' = Just cs
    checkBoxIds _ _ = Nothing
