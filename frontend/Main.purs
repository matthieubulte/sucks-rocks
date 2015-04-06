module Main (main) where

import Network
import Types
import Api
import Views

import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Maybe.Trans
import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

setState = T.asyncSetState <<< runContT

maybeSetState s m = setState $ do
    maybeState <- runMaybeT m
    case maybeState of
         (Just state) -> return state
         Nothing      -> return s

voteAndReload vote (Snippet snippet) = maybeSetState (snippetNotFound snippet.id) $ do
    lift $ vote snippet.id
    snippet <- loadSnippet snippet.id
    return $ oneSnippet snippet

performAction :: forall eff. T.PerformAction Unit Action (T.Action eff State)
performAction _ LoadAllSnippets            = setState $ allSnippets <$> loadSnippets
performAction _ (LoadSnippet id)           = maybeSetState (snippetNotFound id) $ oneSnippet <$> loadSnippet id
performAction _ (VoteSnippetRocks snippet) = voteAndReload voteSnippetRocks snippet
performAction _ (VoteSnippetSucks snippet) = voteAndReload voteSnippetSucks snippet

spec :: T.Spec _ State _ Action
spec = T.simpleSpec loading performAction render
            # T.componentWillMount LoadAllSnippets

main = do
    let component = T.createClass spec
    T.render component unit
