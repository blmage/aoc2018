module Day05 where

import           Conduit
import           Control.Applicative   (liftA2)
import           Control.Monad         ((>=>))
import           Control.Monad.Loops   (untilM_)
import           Control.Monad.State
import           Control.Monad.Trans   (lift)
import           Data.Char             (toLower, toUpper)
import           Data.Foldable         (toList)
import           Data.List             (nub)
import qualified Data.List.PointedList as PL
import           Lens.Micro.Platform

import           Utils.State           (modifyM)


reactChain :: String -> String
reactChain xs = maybe "" toList $ PL.fromList xs >>= \list ->
    flip execStateT list $ flip untilM_ (PL.atEnd <$> get) $ do
        previous <- use PL.focus
        modifyM PL.next
        current  <- use PL.focus
        when (current `reactsWith` previous) $ modifyM react
  where
    react = PL.deleteLeft >=> PL.deleteLeft
    c `reactsWith` c' = c /= c' && toUpper c == toUpper c'


answer1 :: IO Int
answer1 = length . reactChain . init <$> readFile "inputs/day05.txt"

answer2 :: IO Int
answer2 = do
    chain <- reactChain . init <$> readFile "inputs/day05.txt"
    let typeFilters = fmap mkTypeFilter $ nub $ toUpper <$> chain
    pure $ minimum $ (length . reactChain . ($ chain) . filter) <$> typeFilters
  where
    mkTypeFilter :: Char -> (Char -> Bool)
    mkTypeFilter = liftA2 (liftA2 (&&)) (/=) ((/=) . toLower)
