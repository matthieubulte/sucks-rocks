module Utils
    ( fromMaybeT
    ) where

import Data.Maybe
import Control.Monad.Maybe.Trans

fromMaybeT :: forall m a. (Monad m) => a -> MaybeT m a -> m a
fromMaybeT s m = (fromMaybe s) <$> (runMaybeT m)
