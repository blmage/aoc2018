module Utils.State
    (
      modifyM
    ) where

import           Control.Monad.State (StateT, get, put)
import           Control.Monad.Trans (lift)


modifyM :: Monad m => (a -> m a) -> StateT a m ()
modifyM f = get >>= (lift . f) >>= put
