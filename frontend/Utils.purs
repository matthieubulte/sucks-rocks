module Utils
    ( fromMaybeT
    , seq
    , stringify
    ) where

import Data.Maybe
import Control.Monad.Maybe.Trans

fromMaybeT :: forall m a. (Monad m) => a -> MaybeT m a -> m a
fromMaybeT s m = (fromMaybe s) <$> (runMaybeT m)

seq :: forall m a b. (Monad m) => m a -> m b -> m b
seq m k = m >>= \_ -> k

foreign import stringify """
function stringify(a) {
    return JSON.stringify(a);
}
""":: forall a. a -> String

