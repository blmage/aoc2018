{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Day03 where

import           Conduit
import           Control.Applicative  (liftA2)
import           Control.Monad.Except (ExceptT (..), liftEither, runExceptT)
import           Control.Monad.Trans  (lift)
import qualified Data.IntSet          as IS
import           Lens.Micro.Platform
import           Safe                 (headMay)
import           System.IO            (FilePath)
import           Text.Trifecta        hiding (position)

import           Utils


data Position = Position
    { _x :: Int
    , _y :: Int
    }
    deriving (Eq, Show)

makeLenses ''Position


data Size = Size
    { _w :: Int
    , _h :: Int
    }
    deriving (Eq, Show)

makeLenses ''Size


data Claim = Claim
    { _claimId  :: Int
    , _position :: Position
    , _size     :: Size
    }
    deriving (Eq, Show)

makeLenses ''Claim

toIndex :: Size -> Position -> Int
toIndex Size {..} Position {..} = _x + _y * _w


guessFabricSize :: [Claim] -> Size
guessFabricSize = foldr go (Size 0 0)
  where
    go Claim {..} Size {..} =
        Size ((_position ^. x + _size ^. w) `max` _w)
             ((_position ^. y + _size ^. h) `max` _h)


claimParser :: Parser Claim
claimParser = do
    _claimId  <- token $ char '#' *> int
    _         <- symbolic '@'
    _position <- uncurry Position <$> commaSepPair int
    _         <- symbolic ':'
    _size     <- uncurry Size <$> sepByPair int (char 'x')
    pure Claim {..}

claimPositions :: Claim -> [Position]
claimPositions Claim {..} = liftA2 Position [fromX..toX] [fromY..toY]
  where
    fromX = _position ^. x
    toX   = fromX + _size ^. w - 1
    fromY = _position ^. y
    toY   = fromY + _size ^. h - 1

claimIndices :: Size -> Claim -> [Int]
claimIndices size = fmap (toIndex size) . claimPositions

isDisjointTo :: Claim -> Claim -> Bool
isDisjointTo c c' =
       (c  ^. position . x > c' ^. position . x + c' ^. size . w)
    || (c' ^. position . x > c  ^. position . x + c  ^. size . w)
    || (c  ^. position . y > c' ^. position . y + c' ^. size . h)
    || (c' ^. position . y > c  ^. position . y + c  ^. size . h)


fileClaims :: MonadResource m
           => FilePath
           -> ConduitT i o (ExceptT String m) [Claim]
fileClaims path =
       fileLines path
    .| mapMC (liftEither . parseText' claimParser)
    .| sinkList


fabricUsage :: [Claim] -> (IS.IntSet, IS.IntSet)
fabricUsage claims = foldr go (IS.empty, IS.empty) $
    claimIndices (guessFabricSize claims) <$> claims
  where
    go (IS.fromList -> claimSet) (usedSet, overlappedSet) =
        let overlappedSet' = usedSet `IS.intersection` claimSet
        in  ( usedSet       `IS.union` claimSet
            , overlappedSet `IS.union` overlappedSet'
            )


answer1 :: IO (Either String Int)
answer1 = runExceptT $ do
    claims <- ExceptT $ runConduitResExcept $ fileClaims "inputs/day03.txt"
    pure $ IS.size $ snd $ fabricUsage claims

answer2 :: IO (Either String (Maybe Int))
answer2 = runExceptT $ do
    claims <- ExceptT $ runConduitResExcept $ fileClaims "inputs/day03.txt"
    pure $ fmap _claimId $ headMay $ filter (flip all claims . checkPair) claims
  where
    checkPair c c' = c ^. claimId == c' ^. claimId || c `isDisjointTo` c'

answer2' :: IO (Either String (Maybe Int))
answer2' = runExceptT $ do
    claims <- ExceptT $ runConduitResExcept $ fileClaims "inputs/day03.txt"
    pure $ fmap _claimId $ headMay $ findDisjoint claims
  where
    findDisjoint claims = filter isDisjoint claims
      where
        fabricSize    = guessFabricSize claims
        overlappedSet = snd $ fabricUsage claims
        isDisjoint    = IS.disjoint overlappedSet
                      . IS.fromList
                      . claimIndices fabricSize
