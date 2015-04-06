module Main (main) where

import Network
import Types
import Api
import Views

import Control.Monad.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Action as T
import qualified Thermite.Events as T
import qualified Thermite.Types as T

voteAndReload vote (Snippet snippet) = T.asyncSetState <<< runContT $ do
    vote snippet.id
    snippet <- loadSnippet snippet.id
    return $ oneSnippet snippet

performAction :: forall eff. T.PerformAction Unit Action (T.Action eff State)
performAction _ LoadAllSnippets            = T.asyncSetState <<< runContT $ allSnippets <$> loadSnippets
performAction _ (LoadSnippet id)           = T.asyncSetState <<< runContT $ oneSnippet <$> loadSnippet id
performAction _ (VoteSnippetRocks snippet) = voteAndReload voteSnippetRocks snippet
performAction _ (VoteSnippetSucks snippet) = voteAndReload voteSnippetSucks snippet

spec :: T.Spec _ State _ Action
spec = T.simpleSpec loading performAction render
            # T.componentWillMount LoadAllSnippets

main = do
    let component = T.createClass spec
    T.render component unit
