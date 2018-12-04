{-# LANGUAGE RankNTypes #-}

module Utils
    (
      -- * Mapping
      both
    , first3

      -- * Conduit
    , fileLines
    , numberSource
    , runConduitResExcept

      -- * Trifecta
    , int
    , sepByPair
    , commaSepPair
    , parseText
    , parseText'
    ) where

import           Conduit
import           Control.Applicative  (Applicative, liftA2)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.State  (get, put)
import           Data.Bifunctor       (Bifunctor, bimap)
import           Data.Semigroup       ((<>))
import qualified Data.Text            as T
import           Data.Time.Clock
import           System.IO            (FilePath)
import           Text.Trifecta
import qualified Text.Trifecta.Delta  as Trifecta


both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f

first3 :: (a -> d) -> (a, b, c) -> (d, b, c)
first3 f (x, y, z) = (f x, y, z)


fileLines :: MonadResource m => FilePath -> ConduitT i T.Text m ()
fileLines path = sourceFile path .| decodeUtf8LenientC .| linesUnboundedC

numberSource :: Monad m => ConduitT a b m () -> ConduitT a (Integer, b) m ()
numberSource source = evalStateC 0 $ transPipe lift source .| mapMC numberInput
  where
    numberInput x = do
        n <- get
        put $ n +1
        pure (n, x)

runConduitResExcept :: MonadUnliftIO m
                    => ConduitT () Void (ExceptT e (ResourceT m)) a
                    -> m (Either e a)
runConduitResExcept = runResourceT . runExceptT . runConduit


int :: TokenParsing m => m Int
int = fromInteger <$> decimal

sepByPair :: Applicative m => m a -> m sep -> m (a, a)
sepByPair p sep = liftA2 (,) p (sep *> p)

commaSepPair :: TokenParsing m => m a -> m (a, a)
commaSepPair = flip sepByPair comma

parseText :: Parser a -> Trifecta.Delta -> T.Text -> Result a
parseText parser delta = parseString parser delta . T.unpack

parseText' :: Parser a -> T.Text -> Either String a
parseText' = (foldResult (Left . show . _errDoc) pure .) . flip parseText mempty
