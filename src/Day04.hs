{-# LANGUAGE TemplateHaskell #-}

module Day04 where

import           Conduit
import           Control.Applicative  ((<|>))
import           Control.Monad        (mzero)
import           Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import           Data.Bifunctor       (bimap)
import           Data.Function        (on)
import qualified Data.IntMap.Strict   as IM
import           Data.List            (foldl', sort)
import           Data.Maybe           (fromMaybe)
import           Data.Time.Calendar
import           Data.Time.Clock
import           Lens.Micro.Platform
import           Text.Trifecta

import           Utils
import qualified Utils.IntMap         as IM
import           Utils.Time


data EventType = BeginShift Int
               | FallAsleep
               | WakeUp
               deriving (Eq, Ord, Show)

data Event = Event
    { _dateTime  :: !UTCTime
    , _eventType :: !EventType
    }
    deriving (Eq, Ord, Show)

makeLenses ''Event


data Shift = Shift
    { _guardId       :: !Int
    , _startDateTime :: !UTCTime
    , _minutesAsleep :: ![Int]
    }
    deriving (Eq, Show)

makeLenses ''Shift


dateParser :: Parser Day
dateParser = do
    y <- natural
    m <- char '-' *> int
    d <- char '-' *> int
    maybe mzero pure $ fromGregorianValid y m d

timeParser :: Parser DiffTime
timeParser = sepByPair natural (char ':') <&>
    secondsToDiffTime . uncurry (+) . bimap (* 3600) (* 60)

dateTimeParser :: Parser UTCTime
dateTimeParser = UTCTime <$> dateParser <*> (char ' ' *> timeParser)


eventTypeParser :: Parser EventType
eventTypeParser =
        (text "wakes up"     *> pure WakeUp)
    <|> (text "falls asleep" *> pure FallAsleep)
    <|> BeginShift <$> (text "Guard #" *> int <* text " begins shift")

eventParser :: Parser Event
eventParser =
    Event <$> between (char '[') (char ']') dateTimeParser
          <*> (char ' ' *> eventTypeParser)


fileEvents :: MonadResource m
           => FilePath
           -> ConduitT i o (ExceptT String m) [Event]
fileEvents path =
       fileLines path
    .| mapMC (liftEither . parseText' eventParser)
    .| sinkList


eventShifts :: [Event] -> [Shift]
eventShifts = fst . foldl' go ([], Nothing) . sort
  where
    go (shifts, asleepFrom) Event {..} = case _eventType of
        BeginShift id -> (mkShift id : wakeUpShifts, Nothing)
        FallAsleep    -> (shifts, Just _dateTime)
        WakeUp        -> (wakeUpShifts, Nothing)
      where
        mkShift id   = Shift id _dateTime []
        wakeUpShifts = shifts & _head %~ handleWakeUp

        handleWakeUp shift = fromMaybe shift $ do
            fromDateTime      <- asleepFrom
            (toH,   toM,   _) <- diffTimeToDailyTime $ utctDayTime _dateTime
            (fromH, fromM, _) <- diffTimeToDailyTime $ utctDayTime fromDateTime

            let toM'     = if fullHour  then 59 else toM - 1
                fromM'   = if fromH > 0 then 60 else fromM
                fullHour = utctDay fromDateTime < utctDay _dateTime || toH > 0

            pure $ shift & minutesAsleep %~ (++ [fromM'..toM'])


guardDetailedSleepMaps :: [Shift] -> IM.IntMap (IM.IntMap Int)
guardDetailedSleepMaps = flip foldr IM.empty $ \shift ->
    IM.insertWith (IM.unionWith (+)) (shift ^. guardId) $
        IM.fromList $ zip (shift ^. minutesAsleep) (repeat 1)


answer1 :: IO (Either String (Maybe Int))
answer1 = runExceptT $ do
    events <- ExceptT $ runConduitResExcept $ fileEvents "inputs/day04.txt"

    let totalTimeMaps = sum <$> detailedMaps
        detailedMaps  = guardDetailedSleepMaps $ eventShifts events

    pure $ do
        culpritId <- fst <$> IM.maximumPair totalTimeMaps
        sleptTime <- fst <$> IM.maximumPair (detailedMaps IM.! culpritId)
        pure $ culpritId * sleptTime

answer2 :: IO (Either String (Maybe Int))
answer2 = runExceptT $ do
    events <- ExceptT $ runConduitResExcept $ fileEvents "inputs/day04.txt"

    let recordsMap   = IM.maximumPair <$> detailedMaps
        detailedMaps = guardDetailedSleepMaps $ eventShifts events

    pure $ fmap (uncurry (*) . fmap fst)
         $ sequence =<< IM.maximumPairBy (compare `on` maybe 0 snd) recordsMap
