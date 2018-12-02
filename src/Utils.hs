module Utils
    (
      both
    , fileLines
    , numberSource
    ) where

import           Conduit
import           Control.Monad.State (get, put)
import           Data.Bifunctor      (Bifunctor, bimap)
import qualified Data.Text           as T
import           System.IO           (FilePath)


both :: Bifunctor f => (a -> b) -> f a a -> f b b
both f = bimap f f


fileLines :: MonadResource m => FilePath -> ConduitT i T.Text m ()
fileLines path = sourceFile path .| decodeUtf8LenientC .| linesUnboundedC


numberSource :: Monad m => ConduitT a b m () -> ConduitT a (Integer, b) m ()
numberSource source = evalStateC 0 $ transPipe lift source .| mapMC numberInput
  where
    numberInput x = do
        n <- get
        put $ n +1
        pure (n, x)
